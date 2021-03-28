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
    all/0,
    groups/0,
    init_per_group/2,
    end_per_group/2
]).

%% Test cases
-export([
    test_cli/0, test_cli/1,
    auto_help/0, auto_help/1,
    subcmd_help/0, subcmd_help/1,
    missing_handler/0, missing_handler/1,
    bare_cli/0, bare_cli/1,
    multi_module/0, multi_module/1,
    warnings/0, warnings/1,
    simple/0, simple/1,
    malformed_behaviour/0, malformed_behaviour/1,
    remote_cli/0, remote_cli/1
]).

%% Internal exports
-export([
    cli/0,
    cli/1,
    cos/1,
    mul/2
]).

-export([log/2]).

-behaviour(cli).

suite() ->
    [{timetrap, {seconds, 5}}].

all() ->
    [{group, sequence, local}, {group, parallel, remote}].

groups() ->
    [{local, [test_cli, auto_help, subcmd_help, missing_handler, bare_cli,
        multi_module, warnings, malformed_behaviour]}, {remote, [remote_cli]}].

init_per_group(remote, Config) ->
    case is_alive() of
        true ->
            Config;
        false ->
            {ok, NetKernel} = net_kernel:start([?MODULE, shortnames]),
            [{net_kernel, NetKernel} | Config]
    end;
init_per_group(_, Config) ->
    Config.

end_per_group(remote, Config) ->
    net_kernel:stop(),
    proplists:delete(net_kernel, Config);
end_per_group(_, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Helpers

%% OTP logger redirection

log(LogEvent, #{forward := Pid}) ->
    Pid ! {log, LogEvent}.

capture_log(Fun) ->
    Tracer = spawn_link(fun () -> tracer([]) end),
    logger:add_handler(?MODULE, ?MODULE, #{forward => Tracer}),
    Ret =
        try Fun()
        after
            logger:remove_handler(?MODULE)
        end,
    Captured = lists:flatten(lists:reverse(gen_server:call(Tracer, get))),
    {Ret, Captured}.

%% I/O redirection

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
        {log, LogEvent} ->
            tracer([LogEvent | Trace]);
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

capture_output_and_log(Fun) ->
    {{Ret, IO}, Log} = capture_log(fun () -> capture_output(Fun) end),
    {Ret, IO, Log}.

cli_module(Mod, CliRet, FunExport, FunDefs) ->
    Code = [
        io_lib:format("-module(~s).\n", [Mod]),
        "-export([cli/0]).\n",
        if is_list(FunExport) -> lists:flatten(io_lib:format("-export([~s]).\n", [FunExport])); true -> undefined end,
        "-behaviour(cli).\n",
        lists:flatten(io_lib:format("cli() -> ~s.\n", [CliRet])) |
        FunDefs
    ],
    ct:pal("~s~n", [lists:concat(Code)]),
    Tokens = [begin {ok, Tokens, _} = erl_scan:string(C), Tokens end || C <- Code, C =/= undefined],
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
                help => "Sums a list of arguments",
                handler => fun (#{num := Nums}) -> lists:sum(Nums) end,
                arguments => [
                    #{name => num, nargs => nonempty_list, type => int, help => "Numbers to sum"}
                ]
            },
            "math" => #{
                commands => #{
                    "sin" => #{},
                    "cos" => #{handler => {fun (X) -> math:cos(X) end, undefined}, help => "Calculates cosinus"},
                    "extra" => #{commands => #{"ok" => #{}, "fail" => #{}}, handler => optional}
                    },
                arguments => [
                    #{name => in, type => float, help => "Input value"}
                ]
            },
            "mul" => #{
                help => "Multiplies two arguments",
                arguments => [
                    #{name => left, type => int},
                    #{name => right, type => int}
                ]
            },
            "node" => #{
                handler => fun (_) -> io:format("~s~n", [node()]) end
            }
        }
    }.

%%--------------------------------------------------------------------
%% handlers

cli(#{}) ->
    success.

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
    %% test when help => false
    Expected1 = "error: erm sum: required argument missing: num\n",
    {ok, Actual1} = capture_output(fun () -> cli:run(["sum"], #{progname => "erm", help => false}) end),
    ?assertEqual(Expected1, Actual1),
    %% test "catch-all" handler
    ?assertEqual(success, cli:run([])).

auto_help() ->
    [{doc, "Tests automatic --help and -h switch"}].

auto_help(Config) when is_list(Config) ->
    erlang:system_flag(backtrace_depth, 42),
    Expected = "usage: erm  {math|mul|sum}\n\nSubcommands:\n  math \n  mul"
        "  Multiplies two arguments\n  sum  Sums a list of arguments\n",
    ?assertEqual({ok, Expected}, capture_output(fun () -> cli:run(["--help"], #{progname => "erm"}) end)),
    %% add more modules
    cli_module(auto_help, "#{help => \"description\"}", undefined, []),
    Expected1 = "usage: erm  {math|mul|sum}\ndescription\n\nSubcommands:\n  math"
        " \n  mul  Multiplies two arguments\n  sum  Sums a list of arguments\n",
    ?assertEqual({ok, Expected1}, capture_output(fun () -> cli:run(["-h"], #{progname => "erm",
        modules => [?MODULE, auto_help]}) end)),
    %% request help for a subcommand
    Expected2 = "usage: erl math {cos|extra|sin} <in>\n\nSubcommands:\n  cos   "
        "Calculates cosinus\n  extra \n  sin   \n\nArguments:\n  in Input value, float\n",
    ?assertEqual({ok, Expected2}, capture_output(fun () -> cli:run(["math", "--help"],
        #{modules => [?MODULE]}) end)),
    %% request help for a sub-subcommand
    Expected3 = "usage: erl math extra {fail|ok} <in>\n\nSubcommands:\n  fail \n"
        "  ok   \n\nArguments:\n  in Input value, float\n",
    ?assertEqual({ok, Expected3}, capture_output(fun () -> cli:run(["math", "extra", "--help"],
        #{modules => ?MODULE}) end)),
    %% request help for a sub-sub-subcommand
    Expected4 = "usage: erl math cos <in>\n\nArguments:\n  in Input value, float\n",
    ?assertEqual({ok, Expected4}, capture_output(fun () -> cli:run(["math", "cos", "--help"],
        #{modules => ?MODULE}) end)),
    %% request help in a really wrong way (subcommand does not exist)
    Expected5 =
        "error: erl math: invalid argument bad for: in\nusage: erl math {cos|extra|sin} <in>\n\nSubcommands:\n"
        "  cos   Calculates cosinus\n  extra \n  sin   \n\nArguments:\n  in Input value, float\n",
    ?assertEqual({ok, Expected5}, capture_output(fun () -> cli:run(["math", "bad", "--help"],
        #{modules => ?MODULE}) end)).

subcmd_help() ->
    [{doc, "Tests that help for an empty command list does not fail"}].

subcmd_help(Config) when is_list(Config) ->
    CliRet = "#{commands => #{\"foo\" => #{help => \"myfoo\", arguments => [#{name => left, help => \"lefty\"}]}}}",
    cli_module(empty, CliRet, undefined, []),
    %% capture good help output
    {_Ret, IO} = capture_output(fun () -> cli:run(["foo", "--help"], #{modules => empty}) end),
    ?assertEqual("usage: erl foo <left>\n\nArguments:\n  left lefty\n", IO),
    %% capture global help output
    {_Ret1, IO1} = capture_output(fun () -> cli:run(["--help"], #{modules => empty}) end),
    ?assertEqual("usage: erl  {foo}\n\nSubcommands:\n  foo myfoo\n", IO1),
    %% capture broken help output
    {_Ret2, IO2} = capture_output(fun () -> cli:run(["mycool", "--help"], #{modules => [empty]}) end),
    ?assertEqual("error: erl: unrecognised argument: mycool\nusage: erl  {foo}\n\nSubcommands:\n  foo myfoo\n", IO2),
    ct:pal("~s", [IO]).

missing_handler() ->
    [{doc, "Handler can be missing from the module"}].

missing_handler(Config) when is_list(Config) ->
    CliRet = "#{handler => {missing, foobar}}",
    FunExport = "foobar/1", FunDefs = "foobar(#{}) -> success.",
    cli_module(missing, CliRet, FunExport, [FunDefs]),
    {Ret, IO} = capture_output(fun () -> cli:run([], #{modules => [missing, bare, none]}) end),
    ?assertEqual(success, Ret),
    ?assertEqual("", IO).

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
    {ok, IO} = capture_output(fun () -> cli:run(["sum"], #{modules => nomodule}) end),
    ?assertNotEqual(nomatch, string:find(IO, "unrecognised argument: sum")),
    %% ensure log line added
    {ok, IO, Log} = capture_output_and_log(fun () -> cli:run(["sum"], #{modules => nomodule}) end),
    [#{level := Lvl, msg := {Fmt, _}}] = Log,
    ?assertEqual(warning, Lvl),
    ?assertEqual("Error calling ~s:cli(): ~s:~p~n~p", Fmt),
    %% ensure no log line added when suppression is requested
    cli_module(warn, "#{commands => #{\"sum\" => #{}}}", undefined, []),
    {Sum, SumText, LogZero} = capture_output_and_log(fun () -> cli:run(["sum", "0"],
        #{modules => [?MODULE, warn], warn => suppress}) end),
    ?assertEqual("", SumText),
    ?assertEqual(0, Sum),
    ?assertEqual([], LogZero).

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

malformed_behaviour() ->
    [{doc, "Tests for cli/0 callback returning invalid command map"}].

malformed_behaviour(Config) when is_list(Config) ->
    CliRet = "#{commands => #{deploy => #{}}}",
    FunExport = "cli/1", FunDefs = "cli(#{arg := Args}) -> lists:sum(Args).",
    cli_module(malformed, CliRet, FunExport, [FunDefs]),
    {ok, IO, Log} = capture_output_and_log(fun () -> cli:run(["4"], #{modules => malformed}) end),
    ?assertEqual("error: erl: unrecognised argument: 4\nusage: erl\n", IO),
    [#{level := Lvl, msg := {Fmt, _Args}}] = Log,
    ?assertEqual("Error calling ~s:cli(): ~s:~p~n~p", Fmt),
    ?assertEqual(warning, Lvl).

%%--------------------------------------------------------------------
%% Multi-node calls

remote_cli() ->
    [{doc, "Executes CLI code in context of a remote node"}].

remote_cli(Config) when is_list(Config) ->
    Escript = filename:join(proplists:get_value(priv_dir, Config), "cli.escript"),
    %% Args, keep in sync with "rebar.config", XXX: read rebar.config,
    %%  or even better use "rebar3 escriptize" in this test case
    EmuArgs = "+S 1:1 -noinput -hidden -noshell -dist_listen false -connect_all false -sname undefined",
    %% create escript on the fly (pack "argparse + cli")
    Beams = [
        begin
            {ok, Bin} = file:read_file(code:which(Mod)),
            {lists:concat([Mod, ".beam"]), Bin}
        end || Mod <- [argparse, cli]],
    ok = escript:create(Escript, [shebang, comment, {emu_args, EmuArgs}, {archive, Beams, []}]),
    %% call ourselves via escript
    Out = os:cmd("escript " ++ Escript ++ " -n " ++ atom_to_list(node()) ++ " node"),
    ?assertEqual(lists:concat([node(), "\n"]), Out).
