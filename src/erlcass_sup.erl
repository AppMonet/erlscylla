-module(erlcass_sup).

-include("erlcass.hrl").

-behaviour(supervisor).

-export([
    start_link/0,
    start_link/1,
    child_spec/0,
    child_spec/1,
    stop/0,
    init/1,
    erlcass_child_spec/0,
    release_cluster/0
]).

start_link() ->
    start_link(#{}).

start_link(Opts) ->
    ok = ensure_app_loaded(),
    case whereis(?MODULE) of
        undefined ->
            case supervisor:start_link({local, ?MODULE}, ?MODULE, Opts) of
                {ok, Pid} ->
                    {ok, Pid};
                {error, {already_started, Pid}} ->
                    {ok, Pid};
                Other ->
                    Other
            end;
        Pid ->
            {ok, Pid}
    end.

child_spec() ->
    child_spec(#{}).

child_spec(Opts) ->
    #{id => ?MODULE,
      start => {?MODULE, start_link, [Opts]},
      shutdown => infinity,
      type => supervisor,
      modules => [?MODULE]}.

stop() ->
    case whereis(?MODULE) of
        undefined ->
            ok = release_cluster(),
            erlcass_utils:clear_config();
        _Pid ->
            _ = supervisor:stop(?MODULE),
            ok = release_cluster(),
            erlcass_utils:clear_config()
    end.

init(Opts) ->
    _Config = erlcass_utils:set_config(Opts),
    ok = erlcass_stm_cache:create(),
    Children = [
        proccess(erlcass_log, infinity),
        connection_manager()
    ],
    {ok, {{rest_for_one, 10, 1}, Children}}.

proccess(Name, WaitForClose) ->
    {Name, {Name, start_link, []}, permanent, WaitForClose, worker, [Name]}.

connection_manager() ->
    {erlcass_connection_manager, {erlcass_connection_manager, start_link, []}, permanent, 5_000,
        worker, [erlcass_connection_manager]}.

erlcass_child_spec() ->
    proccess(erlcass, infinity).


ensure_app_loaded() ->
    case application:load(erlcass) of
        ok ->
            ok;
        {error, {already_loaded, erlcass}} ->
            ok;
        {error, Reason} ->
            error(Reason)
    end.

release_cluster() ->
    case erlcass_cluster:release() of
        ok ->
            ok;
        {error, _} ->
            ok
    end.
