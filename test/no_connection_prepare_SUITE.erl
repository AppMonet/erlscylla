-module(no_connection_prepare_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-compile(export_all).

all() ->
    [prepare_statement_without_connection].

suite() ->
    [{timetrap, {seconds, 30}}].

init_per_suite(Config) ->
    Backup =
        case application:get_env(erlcass, cluster_options) of
            undefined ->
                undefined;
            Value ->
                Value
        end,
    [{cluster_options_backup, Backup} | Config].

end_per_suite(Config) ->
    catch application:stop(erlcass),
    case proplists:get_value(cluster_options_backup, Config) of
        undefined ->
            application:unset_env(erlcass, cluster_options);
        Original ->
            application:set_env(erlcass, cluster_options, Original)
    end,
    _ = application:start(erlcass),
    ok.

init_per_testcase(_TestCase, Config) ->
    catch application:stop(erlcass),
    application:set_env(erlcass, cluster_options, [
        {contact_points, <<"203.0.113.1">>},
        {connect_timeout, 1000},
        {request_timeout, 1000}
    ]),
    case application:start(erlcass) of
        ok ->
            ok;
        {error, {already_started, erlcass}} ->
            ok;
        Error ->
            ct:fail({failed_to_start_with_unreachable_contact_points, Error})
    end,
    Config.

end_per_testcase(_TestCase, _Config) ->
    catch application:stop(erlcass),
    ok.

prepare_statement_without_connection(_Config) ->
    ct:comment("Attempting to prepare while no Cassandra connections are available"),
    Result = erlcass:add_prepare_statement(no_connection_stmt, <<"SELECT now() FROM system.local">>),
    ?assertMatch({error, _}, Result).
