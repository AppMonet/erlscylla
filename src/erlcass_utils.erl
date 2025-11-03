-module(erlcass_utils).

-include("erlcass.hrl").
-include("erlcass_internals.hrl").

-export([
    set_config/1,
    clear_config/0,
    get_config/0,
    get_env/1,
    get_env/2,
    lookup/2,
    lookup/3
]).

-define(CONFIG_KEY, {?MODULE, config}).

set_config(Opts) ->
    Map = normalize_config(Opts),
    persistent_term:put(?CONFIG_KEY, Map),
    Map.

clear_config() ->
    persistent_term:erase(?CONFIG_KEY),
    ok.

get_config() ->
    persistent_term:get(?CONFIG_KEY, #{}).

get_env(Key) ->
    Map = get_config(),
    case maps:find(Key, Map) of
        {ok, Value} ->
            {ok, Value};
        error ->
            undefined
    end.

get_env(Key, Default) ->
    maps:get(Key, get_config(), Default).

lookup(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        {Key, Result} ->
            Result;
        false ->
            Default
    end.

lookup(Key, List) ->
    lookup(Key, List, null).

normalize_config(#{} = Map) ->
    apply_defaults(Map);
normalize_config(Opts) when is_list(Opts) ->
    apply_defaults(maps:from_list(Opts));
normalize_config(Map) when is_map(Map) ->
    apply_defaults(Map);
normalize_config(_) ->
    apply_defaults(#{}).

apply_defaults(Map) ->
    Defaults = #{
        log_level => ?CASS_LOG_WARN,
        reconnect_delay => ?RECONNECT_DELAY,
        prepared_statements => []
    },
    maps:merge(Defaults, canonicalize(Map)).

canonicalize(Map) ->
    maybe_convert_cluster_options(Map).

maybe_convert_cluster_options(Map) ->
    case maps:find(cluster_options, Map) of
        {ok, Value} ->
            Normalized = case Value of
                V when is_map(V) -> maps:to_list(V);
                V -> V
            end,
            maps:put(cluster_options, Normalized, Map);
        error ->
            Map
    end.
