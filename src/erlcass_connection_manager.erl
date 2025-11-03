-module(erlcass_connection_manager).

-include("erlcass.hrl").
-include("erlcass_internals.hrl").

-behaviour(gen_server).

-export([start_link/0, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(MIN_BACKOFF, 1000).
-define(MAX_BACKOFF, 60000).

start_link() ->
    start_link(#{}).

start_link(Opts) when is_list(Opts) ->
    start_link(maps:from_list(Opts));
start_link(Opts) when is_map(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

init(Opts) ->
    process_flag(trap_exit, true),
    Initial = initial_backoff(),
    State = #{
        backoff => Initial,
        initial_backoff => Initial,
        max_backoff => ?MAX_BACKOFF,
        timer_ref => undefined,
        erlcass_pid => undefined,
        erlcass_monitor => undefined,
        cluster_ready => false,
        prepared_statements => maps:get(prepared_statements, Opts, [])
    },
    {ok, State, 0}.

handle_call(_Request, _From, State) ->
    {reply, {error, unsupported}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    attempt_connect(State);
handle_info(retry, State) ->
    attempt_connect(State);
handle_info({'DOWN', Ref, process, Pid, Reason}, State) ->
    case maps:get(erlcass_monitor, State, undefined) of
        Ref ->
            ?LOG_WARNING("erlcass session ~p terminated: ~s", [Pid, format_reason(Reason)]),
            schedule_retry(
                clear_child(State),
                {session_down, Reason}
            );
        _Other ->
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    _ = cancel_timer(State),
    _ = clear_child(State),
    case maps:get(cluster_ready, State, false) of
        true ->
            ok = erlcass_sup:release_cluster();
        false ->
            ok
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

attempt_connect(State0) ->
    State1 = cancel_timer(State0),
    case ensure_cluster(State1) of
        {ok, State2} ->
            case start_erlcass(State2) of
                {ok, State3} ->
                    case prepare_statements(State3) of
                        {ok, State4} ->
                            ?LOG_INFO("erlcass connection established", []),
                            {noreply, State4};
                        {error, Reason, State4} ->
                            schedule_retry(State4, {prepare_failed, Reason})
                    end;
                {error, Reason, State3} ->
                    schedule_retry(State3, {start_child_failed, Reason})
            end;
        {error, Reason, State2} ->
            schedule_retry(State2, Reason)
    end.

ensure_cluster(State = #{cluster_ready := true}) ->
    {ok, State};
ensure_cluster(State) ->
    ok = erlcass_sup:release_cluster(),
    case configure_cluster() of
        ok ->
            {ok, maps:put(cluster_ready, true, State)};
        {error, Reason} ->
            {error, {cluster_setup_failed, Reason}, State}
    end.

configure_cluster() ->
    case call_erlcass(erlcass_cluster, create, []) of
        ok ->
            case set_log_level() of
                ok ->
                    maybe_set_cluster_options();
                {error, _} = Error ->
                    Error;
                Other ->
                    {error, {set_log_level_failed, Other}}
            end;
        {error, _} = Error ->
            Error;
        Other ->
            {error, {cluster_create_failed, Other}}
    end.

set_log_level() ->
    Level = erlcass_utils:get_env(log_level, ?CASS_LOG_WARN),
    call_erlcass(erlcass_cluster, set_log_level, [Level]).

maybe_set_cluster_options() ->
    case erlcass_utils:get_env(cluster_options) of
        {ok, Options} ->
            call_erlcass(erlcass_cluster, set_options, [Options]);
        undefined ->
            ok
    end.

start_erlcass(State) ->
    Spec = erlcass_sup:erlcass_child_spec(),
    case supervisor:start_child(erlcass_sup, Spec) of
        {ok, Pid} ->
            register_success(Pid, State);
        {error, {already_started, Pid}} ->
            register_success(Pid, State);
        {error, Reason} ->
            {error, Reason, State};
        ignore ->
            {error, ignore, State}
    end.

register_success(Pid, State0) when is_pid(Pid) ->
    State1 = clear_child(State0),
    Ref = erlang:monitor(process, Pid),
    Initial = maps:get(initial_backoff, State1),
    State2 = set_values(State1, [
        {erlcass_pid, Pid},
        {erlcass_monitor, Ref},
        {timer_ref, undefined},
        {backoff, Initial}
    ]),
    {ok, State2};
register_success(_Pid, State) ->
    {error, invalid_pid, State}.

schedule_retry(State0, Reason) ->
    State1 = cancel_timer(clear_child(State0)),
    ok = erlcass_sup:release_cluster(),
    Delay = maps:get(backoff, State1),
    Next = next_backoff(Delay, maps:get(max_backoff, State1)),
    Ref = erlang:send_after(Delay, self(), retry),
    ?LOG_WARNING(
        "failed to establish cassandra session (~s), retrying in ~p ms",
        [format_reason(Reason), Delay]
    ),
    State2 = set_values(State1, [
        {timer_ref, Ref},
        {backoff, Next},
        {cluster_ready, false}
    ]),
    {noreply, State2}.

clear_child(State) ->
    Monitor = maps:get(erlcass_monitor, State, undefined),
    State1 =
        case Monitor of
            undefined ->
                State;
            Ref when is_reference(Ref) ->
                erlang:demonitor(Ref, [flush]),
                maps:put(erlcass_monitor, undefined, State);
            _ ->
                maps:put(erlcass_monitor, undefined, State)
        end,
    maps:put(erlcass_pid, undefined, State1).

cancel_timer(State) ->
    case maps:get(timer_ref, State, undefined) of
        undefined ->
            State;
        Ref ->
            case erlang:cancel_timer(Ref) of
                false ->
                    flush_retry();
                _ ->
                    ok
            end,
            maps:put(timer_ref, undefined, State)
    end.

flush_retry() ->
    receive
        retry ->
            ok
    after 0 ->
            ok
    end.

set_values(State, []) ->
    State;
set_values(State, [{Key, Value} | Rest]) ->
    set_values(maps:put(Key, Value, State), Rest).

initial_backoff() ->
    Delay = erlcass_utils:get_env(reconnect_delay, ?RECONNECT_DELAY),
    case Delay of
        Int when is_integer(Int), Int > 0 ->
            max(Int, ?MIN_BACKOFF);
        _ ->
            ?MIN_BACKOFF
    end.

next_backoff(Delay, Max) ->
    Next = Delay * 2,
    case Next > Max of
        true ->
            Max;
        false when Next < ?MIN_BACKOFF ->
            ?MIN_BACKOFF;
        false ->
            Next
    end.

prepare_statements(State) ->
    Statements = maps:get(prepared_statements, State, []),
    case do_prepare_statements(Statements) of
        ok ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State}
    end.

do_prepare_statements([]) ->
    ok;
do_prepare_statements([{Name, Query} | Rest]) ->
    ?LOG_INFO("preparing scylla statement ~p", [Name]),
    case catch erlcass:add_prepare_statement(Name, Query) of
        ok ->
            do_prepare_statements(Rest);
        {error, already_exist} ->
            do_prepare_statements(Rest);
        {error, Reason} ->
            {error, {Name, Reason}};
        {'EXIT', Reason} ->
            {error, {Name, Reason}};
        Other ->
            {error, {Name, Other}}
    end;
do_prepare_statements(Other) ->
    {error, {invalid_prepared_statements, Other}}.

call_erlcass(Module, Function, Args) ->
    try apply(Module, Function, Args) of
        Result ->
            Result
    catch
        Class:Reason:Stacktrace ->
            {error, {Class, Module, Function, Reason, Stacktrace}}
    end.

format_reason(Reason) ->
    lists:flatten(io_lib:format("~p", [Reason])).
