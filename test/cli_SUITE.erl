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
    test_cli/0, test_cli/1,
    auto_help/0, auto_help/1,
    bare_cli/0, bare_cli/1,
    multi_module/0, multi_module/1,
    warnings/0, warnings/1,
    simple/0, simple/1
]).

%% Internal exports
-export([
    cli/0,
    cli/1,
    sum/1,
    cos/1,
    mul/2
]).

-behaviour(cli).

suite() ->
    [{timetrap, {seconds, 5}}].

all() ->
    [test_cli, auto_help, bare_cli, multi_module, warnings].

%%--------------------------------------------------------------------
%% Helpers

%% {io_request, From, ReplyAs, Request}
%% {io_reply, ReplyAs, Reply}

tracer(Trace) ->
    receive
        {io_request, From, ReplyAs, {put_chars, _Encoding, Characters}} ->
            From ! {io_reply, ReplyAs, ok},
            tracer([Characters | Trace]);
        {io_request, From, ReplyAs, {put_chars, _Encoding, Module, Function, Args}} ->
            Text = erlang:apply(Module, Function, Args),
            From ! {io_reply, ReplyAs, ok},
            tracer([Text | Trace]);
        {'$gen_call', From, get} ->
            gen:reply(From, Trace);
        Other ->
            ct:pal("Unexpected I/O request: ~p", [Other]),
            tracer(Trace)
    end.

capture_output(Fun) ->
    OldLeader = group_leader(),
    Tracer = spawn_link(fun () -> tracer([]) end),
    true = group_leader(Tracer, self()),
    Ret = try Fun()
        after
            group_leader(OldLeader, self())
        end,
    Captured = lists:flatten(lists:reverse(gen_server:call(Tracer, get))),
    {Ret, Captured}.

cli_module(Mod, CliRet, FunExport, FunDefs) ->
    Code = [
        io_lib:format("-module(~s).", [Mod]),
        "-export([cli/0]).",
        lists:flatten(io_lib:format("-export([~s]).", [FunExport])),
        "-behaviour(cli).",
        lists:flatten(io_lib:format("cli() -> ~s.", [CliRet])) |
        FunDefs
    ],
    ct:pal("~s~n", [Code]),
    Tokens = [begin {ok, Tokens, _} = erl_scan:string(C), Tokens end || C <- Code],
    %ct:pal("~p", [Tokens]),
    Forms = [begin {ok, F} = erl_parse:parse_form(T), F end || T <- Tokens],
    %ct:pal("~p", [Forms]),
    {ok, Mod, Bin} = compile:forms(Forms),
    {module, Mod} = code:load_binary(Mod, atom_to_list(Mod) ++ ".erl", Bin).

cli_module(Mod, Calc) ->
    CliRet = lists:flatten(
        io_lib:format("#{commands => #{\"~s\" => #{arguments => [#{name => arg, nargs => list, type => int}]}}}", [Mod])),
    FunExport = lists:flatten(io_lib:format("~s/1", [Mod])),
    FunDefs = lists:flatten(
        io_lib:format("~s(#{arg := Args}) -> ~s(Args).", [Mod, Calc])),
    cli_module(Mod, CliRet, FunExport, [FunDefs]).

%%--------------------------------------------------------------------
%% Command Map definition

cli() ->
    #{
        handler => optional,
        commands => #{
            "sum" => #{
                arguments => [
                    #{name => num, nargs => nonempty_list, type => int, help => "Numbers to sum"}
                ]
            },
            "math" => #{
                commands => #{
                    "sin" => #{},
                    "cos" => #{}
                    },
                arguments => [
                    #{name => in, type => float, help => "Input value"}
                ]
            },
            "mul" => #{
                arguments => [
                    #{name => left, type => int},
                    #{name => right, type => int}
                ]
            }
        }
    }.

%%--------------------------------------------------------------------
%% handlers

cli(#{}) ->
    success.

sum(#{num := Nums}) ->
    lists:sum(Nums).

cos(#{in := In}) ->
    math:cos(In).

mul(Left, Right) ->
    Left * Right.

%%--------------------------------------------------------------------
%% TEST CASES

test_cli() ->
    [{doc, "Tests CLI commands"}].

test_cli(Config) when is_list(Config) ->
    ?assertEqual(math:cos(3.14), cli:run(["math", "cos", "3.14"])),
    ?assertEqual(9, cli:run(["mul", "3", "3"], #{modules => [?MODULE], default => undefined})),
    ?assertEqual(4, cli:run(["sum", "2", "2"])),
    ?assertEqual(6, cli:run(["sum", "3", "3"], #{modules => ?MODULE})),
    ?assertEqual(6, cli:run(["sum", "3", "3"], #{modules => [?MODULE]})),
    Expected = "error: erm sum: required argument missing: num\nusage: erm",
    {ok, Actual} = capture_output(fun () -> cli:run(["sum"], #{progname => "erm"}) end),
    ?assertEqual(Expected, lists:sublist(Actual, length(Expected))),
    %% test "catch-all" handler
    ?assertEqual(success, cli:run([])).

auto_help() ->
    [{doc, "Tests automatic --help and -h switch"}].

auto_help(Config) when is_list(Config) ->
    Expected = "usage: erm  {math|mul|sum}\n\nSubcommands:\n  math \n  mul  \n  sum  \n",
    ?assertEqual({ok, Expected}, capture_output(fun () -> cli:run(["--help"], #{progname => "erm"}) end)).

bare_cli() ->
    [{doc, "Bare cli, no sub-commands"}].

bare_cli(Config) when is_list(Config) ->
    CliRet = "#{arguments => [#{name => arg, nargs => list, type => int}]}",
    FunExport = "cli/1",
    FunDefs = "cli(#{arg := Args}) -> lists:sum(Args).",
    cli_module(bare, CliRet, FunExport, [FunDefs]),
    {Ret, IO} = capture_output(fun () -> cli:run(["4", "7"], #{modules => bare}) end),
    ct:pal("~s", [IO]),
    ?assertEqual(11, Ret),
    %% check usage/help working, and not starting with "error: "
    cli_module(bad, "#{arguments => [#{name => arg, short => $s}]}", "none/0", ["none() -> ok."]),
    Expected = "usage: ",
    {ok, Usage} = capture_output(fun () -> cli:run([], #{modules => bad}) end),
    ?assertEqual(Expected, lists:sublist(Usage, length(Expected))).

multi_module() ->
    [{doc, "Creates several modules, for cli interface to work"}].

multi_module(Config) when is_list(Config) ->
    cli_module(sum, "lists:sum"), %% funny enough, this causes a duplicate definition!
    cli_module(max, "lists:max"),
    ?assertEqual(3, cli:run(["sum", "1", "2"])),
    ?assertEqual(20, cli:run(["max", "10", "20"])).

warnings() ->
    [{doc, "Ensure warnings are skipped, or emitted"}].

warnings(Config) when is_list(Config) ->
    %% TODO: implement logger handler that intercepts ?LOG_WARNING calls
    {ok, IO} = capture_output(fun () -> cli:run(["sum"], #{modules => nomodule}) end),
    ?assertNotEqual(nomatch, string:find(IO, "unrecognised argument: sum")),
    %% ensure no log line added
    {ok, IO} = capture_output(fun () -> cli:run(["sum"], #{modules => nomodule, warn => suppress}) end).

simple() ->
    [{doc, "Runs simple example from examples"}].

simple(Config) when is_list(Config) ->
    CliRet = "#{arguments => [#{name => force, short => $f, type => boolean, default => false},"
        "#{name => recursive, short => $r, type => boolean, default => false},"
        "#{name => dir}]}",
    FunExport = "cli/3",
    FunDefs = "cli(Force, Recursive, Dir) -> io:format(\"Removing ~s (force: ~s, recursive: ~s)~n\",   [Dir, Force, Recursive]).",
    cli_module(simple, CliRet, FunExport, [FunDefs]),
    {ok, IO} = capture_output(fun () -> cli:run(["4"], #{modules => simple, default => undefined}) end),
    ct:pal("~s", [IO]),
    ?assertEqual("Removing 4 (force: false, recursive: false)\n", IO).
