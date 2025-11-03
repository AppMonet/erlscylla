-module(no_connection_prepare_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-compile(export_all).

all() ->
    [prepare_statement_without_session_connections].

suite() ->
    [{timetrap, {seconds, 10}}].

prepare_statement_without_session_connections(_Config) ->
    {ok, Session} = erlcass_nif:cass_session_new(),
    Tag = make_ref(),
    Result = erlcass_nif:cass_session_prepare(Session, self(), {<<"SELECT now() FROM system.local">>, []}, Tag),
    ?assertMatch({error, _}, Result).
