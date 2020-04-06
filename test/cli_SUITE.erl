%%%-------------------------------------------------------------------
%%% @author Maxim Fedorov <maximfca@gmail.com>
%%% @doc
%%% cli: test suite to provide CLI functionality for escript
%%% @end
-module(cli_SUITE).
-author("maximfca@gmail.com").

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% API exports

%% Test server callbacks
-export([
    suite/0,
    all/0
]).

%% Test cases
-export([
    test_cli/0, test_cli/1
]).

%% Internal exports
-export([
    cli/0,
    sum/1,
    cos/1
]).

-behaviour(cli).

suite() ->
    [{timetrap, {seconds, 5}}].

all() ->
    [test_cli].

%%--------------------------------------------------------------------
%% Helpers

cli() ->
    #{
        commands => #{
            "sum" => #{
                arguments => [
                    #{name => num, nargs => nonempty_list, type => int, help => "Numbers to sum"}
                ]
            },
            "cos" => #{
                arguments => [
                    #{name => in, type => float, help => "Input value"}
                ]
            }
        }
    }.

%%--------------------------------------------------------------------
%% handlers

sum(#{num := Nums}) ->
    lists:sum(Nums).

cos(#{in := In}) ->
    math:cos(In).

%%--------------------------------------------------------------------
%% TEST CASES

test_cli() ->
    [{doc, "Tests CLI commands"}].

test_cli(Config) when is_list(Config) ->
    ?assertEqual(4, cli:run(["sum", "2", "2"])),
    ?assertEqual(math:cos(3.14), cli:run(["cos", "3.14"])).
