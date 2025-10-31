-module(erlcass_stm_sessions).

-export([
    create/0,
    reset/0,
    set/3,
    get/1
]).

-define(ETS_PREPARED_STM_SESSIONS, erlcass_ets_prepared_stm_sessions).

create() ->
    case ets:info(?ETS_PREPARED_STM_SESSIONS) of
        undefined ->
            ?ETS_PREPARED_STM_SESSIONS = ets:new(?ETS_PREPARED_STM_SESSIONS, [set, named_table, protected, {read_concurrency, true}]),
            ok;
        _ ->
            ok
    end.

reset() ->
    case ets:info(?ETS_PREPARED_STM_SESSIONS) of
        undefined ->
            ok;
        _ ->
            ets:delete_all_objects(?ETS_PREPARED_STM_SESSIONS),
            ok
    end.

set(Identifier, Session, StatementRef) ->
    true = ets:insert(?ETS_PREPARED_STM_SESSIONS, {Identifier, {Session, StatementRef}}).

get(Identifier) ->
    case ets:lookup(?ETS_PREPARED_STM_SESSIONS, Identifier) of
        [{Identifier, Value}] ->
            Value;
        [] ->
            undefined
    end.
