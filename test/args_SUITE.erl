%%%-------------------------------------------------------------------
%%% @copyright (C) 2020-2021, Maxim Fedorov <maximfca@mail.com>
%%% @doc
%%%  Tests for argparse library.
%%% @end
-module(args_SUITE).
-author("maximfca@gmail.com").

-export([suite/0, all/0, groups/0]).

-export([
    basic/0, basic/1,
    long_form_eq/0, long_form_eq/1,
    single_arg_built_in_types/0, single_arg_built_in_types/1,
    complex_command/0, complex_command/1,
    unicode/0, unicode/1,
    errors/0, errors/1,
    args/0, args/1,
    argparse/0, argparse/1,
    negative/0, negative/1,
    nodigits/0, nodigits/1,
    python_issue_15112/0, python_issue_15112/1,
    default_for_not_required/0, default_for_not_required/1,
    global_default/0, global_default/1,
    type_validators/0, type_validators/1,
    error_format/0, error_format/1,
    subcommand/0, subcommand/1,
    very_short/0, very_short/1,
    multi_short/0, multi_short/1,
    proxy_arguments/0, proxy_arguments/1,
    usage/0, usage/1,
    usage_required_args/0, usage_required_args/1,
    readme/0, readme/1,
    error_usage/0, error_usage/1,
    meta/0, meta/1,
    usage_template/0, usage_template/1
]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").


suite() ->
    [{timetrap, {seconds, 5}}].

groups() ->
    [{parallel, [parallel], [
        basic, long_form_eq, single_arg_built_in_types, complex_command, errors,
        unicode, args, args, negative, proxy_arguments, default_for_not_required,
        nodigits,  python_issue_15112, type_validators, subcommand, error_format,
        very_short, multi_short, usage, readme, error_usage, meta, usage_template,
        global_default
    ]}].

all() ->
    [{group, parallel}].

%%--------------------------------------------------------------------
%% Helpers

prog() ->
    {ok, [[ProgStr]]} = init:get_argument(progname), ProgStr.

make_error(CmdLine, CmdMap) ->
    try parse(CmdLine, CmdMap), exit(must_never_succeed)
    catch error:{args, Reason} ->
        args:format_error(Reason)
    end.

parse_opts(Args, Opts) ->
    args:parse(string:lexemes(Args, " "), #{arguments => Opts}).

parse(Args, Command) ->
    args:parse(string:lexemes(Args, " "), Command).

parse_cmd(Args, Command) ->
    args:parse(string:lexemes(Args, " "), #{commands => Command}).

%% ubiquitous command - contains *every* combination
ubiq_cmd() ->
    #{
        arguments => [
            #{name => r, short => $r, type => boolean, help => "recursive"},
            #{name => f, short => $f, type => boolean, long => "-force", help => "force"},
            #{name => v, short => $v, type => boolean, action => count, help => "verbosity level"},
            #{name => interval, short => $i, type => {int, [{min, 1}]}, help => "interval set"},
            #{name => weird, long => "-req", help => "required optional, right?"},
            #{name => float, long => "-float", type => float, default => 3.14, help => "floating-point long form argument"}
        ],
        commands => #{
            "start" => #{help => "verifies configuration and starts server",
                arguments => [
                    #{name => server, help => "server to start"},
                    #{name => shard, short => $s, type => int, nargs => nonempty_list, help => "initial shards"},
                    #{name => part, short => $p, type => int, nargs => list, help => hidden},
                    #{name => z, short => $z, type => {int, [{min, 1}, {max, 10}]}, help => "between"},
                    #{name => l, short => $l, type => {int, [{max, 10}]}, nargs => 'maybe', help => "maybe lower"},
                    #{name => more, short => $m, type => {int, [{max, 10}]}, help => "less than 10"},
                    #{name => optpos, required => false, type => {int, []}, help => "optional positional"},
                    #{name => bin, short => $b, type => {binary, <<"m">>}, help => "binary with re"},
                    #{name => g, short => $g, type => {binary, <<"m">>, []}, help => "binary with re"},
                    #{name => t, short => $t, type => {string, "m"}, help => "string with re"},
                    #{name => e, long => "--maybe-req", required => true, type => int, nargs => 'maybe', help => "maybe required int"},
                    #{name => y, required => true, long => "-yyy", short => $y, type => {string, "m", []}, help => "string with re"},
                    #{name => u, short => $u, type => {string, ["1", "2"]}, help => "string choices"},
                    #{name => choice, short => $c, type => {int, [1,2,3]}, help => "tough choice"},
                    #{name => fc, short => $q, type => {float, [2.1,1.2]}, help => "floating choice"},
                    #{name => ac, short => $w, type => {atom, [one, two]}, help => "atom choice"},
                    #{name => au, long => "-unsafe", type => {atom, unsafe}, help => "unsafe atom"},
                    #{name => as, long => "-safe", type => atom, help => "safe atom"},
                    #{name => name, required => false, nargs => list, help => hidden},
                    #{name => long, long => "foobar", required => false, help => "foobaring option"}
                ], commands => #{
                    "crawler" => #{arguments => [
                        #{name => extra, long => "--extra", help => "extra option very deep"}
                    ],
                        help => "controls crawler behaviour"},
                    "doze" => #{help => "dozes a bit"}}
            },
            "stop" => #{help => "stops running server", arguments => []
            },
            "status" => #{help => "prints server status", arguments => [],
                commands => #{
                    "crawler" => #{
                        arguments => [#{name => extra, long => "--extra", help => "extra option very deep"}],
                        help => "crawler status"}}
            },
            "restart" => #{help => hidden, arguments => [
                #{name => server, help => "server to restart"},
                #{name => duo, short => $d, long => "-duo", help => "dual option"}
            ]}
    }
    }.

%%--------------------------------------------------------------------
%% Test Cases

readme() ->
    [{doc, "Test cases covered in README.md"}].

readme(Config) when is_list(Config) ->
    Rm = #{
        arguments => [
            #{name => dir},
            #{name => force, short => $f, type => boolean, default => false},
            #{name => recursive, short => $r, type => boolean}
        ]
    },
    ?assertEqual(#{dir => "dir", force => true, recursive => true},
        args:parse(["-rf", "dir"], Rm, #{result => argmap})),
    %% override progname
    ?assertEqual("usage: readme\n",
        args:help(#{}, #{progname => "readme"})),
    ?assertEqual("usage: readme\n",
        args:help(#{}, #{progname => readme})),
    ?assertException(error, {args,
        {invalid_command, [], progname, "progname must be a list or an atom"}},
        args:help(#{}, #{progname => 123})),
    %% command example
    Cmd = #{
        commands => #{"sub" => #{}},
        arguments => [#{name => pos}]
    },
    ?assertEqual(parse("opt sub", Cmd), parse("sub opt", Cmd)).

basic() ->
    [{doc, "Basic cases"}].

basic(Config) when is_list(Config) ->
    %% empty command, with full options path
    ?assertMatch({#{}, {["cmd"],#{}}},
        args:parse(["cmd"], #{commands => #{"cmd" => #{}}}, #{result => full})),
    %% sub-command, with no path, but user-supplied argument
    ?assertEqual({#{},{["cmd", "sub"],#{attr => pos}}},
        args:parse(["cmd", "sub"], #{commands => #{"cmd" => #{commands => #{"sub" => #{attr => pos}}}}})),
    %% command with positional argument
    PosCmd = #{arguments => [#{name => pos}]},
    ?assertEqual({#{pos => "arg"}, {["cmd"], PosCmd}},
        args:parse(["cmd", "arg"], #{commands => #{"cmd" => PosCmd}})),
    %% command with optional argument
    OptCmd = #{arguments => [#{name => force, short => $f, type => boolean}]},
    ?assertEqual({#{force => true}, {["rm"], OptCmd}},
        parse(["rm -f"], #{commands => #{"rm" => OptCmd}}), "rm -f"),
    %% command with optional and positional argument
    PosOptCmd = #{arguments => [#{name => force, short => $f, type => boolean}, #{name => dir}]},
    ?assertEqual({#{force => true, dir => "dir"}, {["rm"], PosOptCmd}},
        parse(["rm -f dir"], #{commands => #{"rm" => PosOptCmd}}), "rm -f dir"),
    %% no command, just argument list
    Kernel = #{name => kernel, long => "kernel", type => atom, nargs => 2},
    ?assertEqual(#{kernel => [port, dist]},
        parse(["-kernel port dist"], #{arguments => [Kernel]})),
    %% same but positional
    ArgList = #{name => arg, nargs => 2, type => boolean},
    ?assertEqual(#{arg => [true, false]},
        parse(["true false"], #{arguments => [ArgList]})).

long_form_eq() ->
    [{doc, "Tests that long form supports --arg=value"}].

long_form_eq(Config) when is_list(Config) ->
    %% cmd --arg=value
    PosOptCmd = #{arguments => [#{name => arg, long => "-arg"}]},
    ?assertEqual({#{arg => "value"}, {["cmd"], PosOptCmd}},
        parse(["cmd --arg=value"], #{commands => #{"cmd" => PosOptCmd}})),
    %% --int=10
    ?assertEqual(#{int => 10}, parse(["--int=10"], #{arguments => [#{name => int, type => int, long => "-int"}]})).

single_arg_built_in_types() ->
    [{doc, "Tests all built-in types supplied as a single argument"}].

% built-in types testing
single_arg_built_in_types(Config) when is_list(Config) ->
    Bool = #{arguments => [#{name => meta, type => boolean, short => $b, long => "-boolean"}]},
    ?assertEqual(#{}, parse([""], Bool)),
    ?assertEqual(#{meta => true}, parse(["-b"], Bool)),
    ?assertEqual(#{meta => true}, parse(["--boolean"], Bool)),
    ?assertEqual(#{meta => false}, parse(["--boolean false"], Bool)),
    %% integer tests
    Int = #{arguments => [#{name => int, type => int, short => $i, long => "-int"}]},
    ?assertEqual(#{int => 1}, parse([" -i 1"], Int)),
    Prog = prog(),
    ?assertException(error, {args,{invalid_argument,[Prog],int,"1,"}}, parse([" -i 1, 3"], Int)),
    ?assertEqual(#{int => 1}, parse(["--int 1"], Int)),
    ?assertEqual(#{int => -1}, parse(["-i -1"], Int)),
    %% floating point
    Float = #{arguments => [#{name => f, type => float, short => $f}]},
    ?assertEqual(#{f => 44.44}, parse(["-f 44.44"], Float)),
    %% atoms, existing
    Atom = #{arguments => [#{name => atom, type => atom, short => $a, long => "-atom"}]},
    ?assertEqual(#{atom => atom}, parse(["-a atom"], Atom)),
    ?assertEqual(#{atom => atom}, parse(["--atom atom"], Atom)).

type_validators() ->
    [{doc, "Validators for built-in types"}].

type_validators(Config) when is_list(Config) ->
    %% {float, [{min, float()} | {max, float()}]} |
    Prog = [prog()],
    ?assertException(error, {args, {invalid_argument,Prog,float, 0.0}},
        parse_opts("0.0", [#{name => float, type => {float, [{min, 1.0}]}}])),
    ?assertException(error, {args, {invalid_argument,Prog,float, 2.0}},
        parse_opts("2.0", [#{name => float, type => {float, [{max, 1.0}]}}])),
    %% {int, [{min, integer()} | {max, integer()}]} |
    ?assertException(error, {args, {invalid_argument,Prog,int, 10}},
        parse_opts("10", [#{name => int, type => {int, [{min, 20}]}}])),
    ?assertException(error, {args, {invalid_argument,Prog,int, -5}},
        parse_opts("-5", [#{name => int, type => {int, [{max, -10}]}}])),
    %% string: regex & regex with options
    %% {string, string()} | {string, string(), []}
    ?assertException(error, {args, {invalid_argument,Prog,str, "me"}},
        parse_opts("me", [#{name => str, type => {string, "me.me"}}])),
    ?assertException(error, {args, {invalid_argument,Prog,str, "me"}},
        parse_opts("me", [#{name => str, type => {string, "me.me", []}}])),
    %% {binary, {re, binary()} | {re, binary(), []}
    ?assertException(error, {args, {invalid_argument,Prog, bin, "me"}},
        parse_opts("me", [#{name => bin, type => {binary, <<"me.me">>}}])),
    ?assertException(error, {args, {invalid_argument,Prog,bin, "me"}},
        parse_opts("me", [#{name => bin, type => {binary, <<"me.me">>, []}}])),
    %% now successful regexes
    ?assertEqual(#{str => "me"},
        parse_opts("me", [#{name => str, type => {string, "m."}}])),
    ?assertEqual(#{str => "me"},
        parse_opts("me", [#{name => str, type => {string, "m.", []}}])),
    ?assertEqual(#{str => "me"},
        parse_opts("me", [#{name => str, type => {string, "m.", [{capture, none}]}}])),
    %% and for binary too...
    ?assertEqual(#{bin => <<"me">>},
        parse_opts("me", [#{name => bin, type => {binary, <<"m.">>}}])),
    ?assertEqual(#{bin => <<"me">>},
        parse_opts("me", [#{name => bin, type => {binary, <<"m.">>, []}}])),
    ?assertEqual(#{bin => <<"me">>},
        parse_opts("me", [#{name => bin, type => {binary, <<"m.">>, [{capture, none}]}}])),
    %% more successes
    ?assertEqual(#{int => 5},
        parse_opts("5", [#{name => int, type => {int, [{min, 0}, {max, 10}]}}])),
    ?assertEqual(#{bin => <<"5">>},
        parse_opts("5", [#{name => bin, type => binary}])),
    ?assertEqual(#{str => "011"},
        parse_opts("11", [#{name => str, type => {custom, fun(S) -> [$0|S] end}}])),
    %% %% funny non-atom-atom: ensure the atom does not exist
    ?assertException(error, badarg, list_to_existing_atom("$can_never_be")),
    ArgMap = parse_opts("$can_never_be", [#{name => atom, type => {atom, unsafe}}]),
    args:validate(#{arguments => [#{name => atom, type => {atom, unsafe}}]}),
    %% must be successful, but really we can't create an atom in code!
    ?assertEqual(list_to_existing_atom("$can_never_be"), maps:get(atom, ArgMap)),
    %% choices: exceptions
    ?assertException(error, {args, {invalid_argument, Prog, bin, "K"}},
        parse_opts("K", [#{name => bin, type => {binary, [<<"M">>, <<"N">>]}}])),
    ?assertException(error, {args, {invalid_argument, Prog, str, "K"}},
        parse_opts("K", [#{name => str, type => {string, ["M", "N"]}}])),
    ?assertException(error, {args, {invalid_argument, Prog, atom, "K"}},
        parse_opts("K", [#{name => atom, type => {atom, [one, two]}}])),
    ?assertException(error, {args, {invalid_argument, Prog, int, 12}},
        parse_opts("12", [#{name => int, type => {int, [10, 11]}}])),
    ?assertException(error, {args, {invalid_argument, Prog, float, 1.3}},
        parse_opts("1.3", [#{name => float, type => {float, [1.2, 1.4]}}])),
    %% choices: valid
    ?assertEqual(#{bin => <<"K">>},
        parse_opts("K", [#{name => bin, type => {binary, [<<"M">>, <<"K">>]}}])),
    ?assertEqual(#{str => "K"},
        parse_opts("K", [#{name => str, type => {string, ["K", "N"]}}])),
    ?assertEqual(#{atom => one},
        parse_opts("one", [#{name => atom, type => {atom, [one, two]}}])),
    ?assertEqual(#{int => 12},
        parse_opts("12", [#{name => int, type => {int, [10, 12]}}])),
    ?assertEqual(#{float => 1.3},
        parse_opts("1.3", [#{name => float, type => {float, [1.3, 1.4]}}])),
    ok.

complex_command() ->
    [{doc, "Parses a complex command that has a mix of optional and positional arguments"}].

complex_command(Config) when is_list(Config) ->
    Command = #{arguments => [
        %% options
        #{name => string, short => $s, long => "-string", action => append, help => "String list option"},
        #{name => boolean, type => boolean, short => $b, action => append, help => "Boolean list option"},
        #{name => float, type => float, short => $f, long => "-float", action => append, help => "Float option"},
        %% positional args
        #{name => integer, type => int, help => "Integer variable"},
        #{name => string, help => "alias for string option", action => extend, nargs => list}
    ]},
    CmdMap = #{commands => #{"start" => Command}},
    Parsed = args:parse(string:lexemes("start --float 1.04 -f 112 -b -b -s s1 42 --string s2 s3 s4", " "), CmdMap),
    Expected = #{float => [1.04, 112], boolean => [true, true], integer => 42, string => ["s1", "s2", "s3", "s4"]},
    ?assertEqual({Expected, {["start"], Command}}, Parsed).

unicode() ->
    [{doc, "Ensure unicode support"}].

unicode(Config) when is_list(Config) ->
    %% test unicode short & long
    ?assertEqual(#{one => true}, parse(["-Ф"], #{arguments => [#{name => one, short => $Ф, type => boolean}]})),
    ?assertEqual(#{long => true}, parse(["--åäö"], #{arguments => [#{name => long, long => "-åäö", type => boolean}]})),
    %% test default, help and value in unicode
    Cmd = #{arguments => [#{name => text, type => binary, help => "åäö", default => <<"★"/utf8>>}]},
    Expected = #{text => <<"★"/utf8>>},
    ?assertEqual(Expected, args:parse([], Cmd)), %% default
    ?assertEqual(Expected, args:parse(["★"], Cmd)), %% specified in the command line
    ?assertEqual("usage: erl <text>\n\nArguments:\n  text åäö (binary, ★)\n", args:help(Cmd)),
    %% test command name and argument name in unicode
    Uni = #{commands => #{"åäö" => #{help => "öФ"}}, handler => optional,
        arguments => [#{name => "Ф", short => $ä, long => "åäö"}]},
    UniExpected = "usage: erl  {åäö} [-ä <Ф>] [-åäö <Ф>]\n\nSubcommands:\n  åäö      öФ\n\nOptional arguments:\n  -ä, -åäö Ф\n",
    ?assertEqual(UniExpected, args:help(Uni)),
    ParsedExpected = #{"Ф" => "öФ"},
    ?assertEqual(ParsedExpected, args:parse(["-ä", "öФ"], Uni)).

errors() ->
    [{doc, "Tests for various errors, missing arguments etc"}].

errors(Config) when is_list(Config) ->
    Prog = [prog()],
    %% conflicting option names
    ?assertException(error, {args, {invalid_option, _, two, short, "short conflicting with one"}},
        parse("", #{arguments => [#{name => one, short => $$}, #{name => two, short => $$}]})),
    ?assertException(error, {args, {invalid_option, _, two, long, "long conflicting with one"}},
        parse("", #{arguments => [#{name => one, long => "a"}, #{name => two, long => "a"}]})),
    %% broken options
    %% long must be a string
    ?assertException(error, {args, {invalid_option, _, one, long, _}},
        parse("", #{arguments => [#{name => one, long => ok}]})),
    %% short must be a printable character
    ?assertException(error, {args, {invalid_option, _, one, short, _}},
        parse("", #{arguments => [#{name => one, short => ok}]})),
    ?assertException(error, {args, {invalid_option, _, one, short, _}},
        parse("", #{arguments => [#{name => one, short => 7}]})),
    %% required is a boolean
    ?assertException(error, {args, {invalid_option, _, one, required, _}},
        parse("", #{arguments => [#{name => one, required => ok}]})),
    ?assertException(error, {args, {invalid_option, _, one, help, _}},
        parse("", #{arguments => [#{name => one, help => ok}]})),
    %% broken commands
    ?assertException(error, {args, {invalid_command, _, commands, _}},
        parse("", #{commands => ok})),
    ?assertException(error, {args, {invalid_command, _, commands, _}},
        parse("", #{commands => #{ok => #{}}})),
    ?assertException(error, {args, {invalid_command, _, help, _}},
        parse("", #{commands => #{"ok" => #{help => ok}}})),
    ?assertException(error, {args, {invalid_command, _, handler, _}},
        parse("", #{commands => #{"ok" => #{handler => fun errors/0}}})),
    %% unknown option at the top of the path
    ?assertException(error, {args, {unknown_argument, Prog, "arg"}},
        parse_cmd(["arg"], #{})),
    %% positional argument missing
    Opt = #{name => mode, required => true},
    ?assertException(error, {args, {missing_argument, _, mode}},
        parse_cmd(["start"], #{"start" => #{arguments => [Opt]}})),
    %% optional argument missing
    Opt1 = #{name => mode, short => $o, required => true},
    ?assertException(error, {args, {missing_argument, _, mode}},
        parse_cmd(["start"], #{"start" => #{arguments => [Opt1]}})),
    %% atom that does not exist
    Opt2 = #{name => atom, type => atom},
    ?assertException(error, {args, {invalid_argument, _, atom, "boo-foo"}},
        parse_cmd(["start boo-foo"], #{"start" => #{arguments => [Opt2]}})),
    %% optional argument missing some items
    Opt3 = #{name => kernel, long => "kernel", type => atom, nargs => 2},
    ?assertException(error, {args, {invalid_argument, _, kernel, ["port"]}},
        parse_cmd(["start -kernel port"], #{"start" => #{arguments => [Opt3]}})),
    %% not-a-list of arguments
    ?assertException(error, {args, {invalid_command, _, commands,"options must be a list"}},
        parse_cmd([], #{"start" => #{arguments => atom}})),
    %% command is not a map
    ?assertException(error, {args, {invalid_command, _, commands,"command description must be a map"}},
        parse_cmd([], #{"start" => []})),
    %% positional argument missing some items
    Opt4 = #{name => arg, type => atom, nargs => 2},
    ?assertException(error, {args, {invalid_argument, _, arg, ["p1"]}},
    parse_cmd(["start p1"], #{"start" => #{arguments => [Opt4]}})).

args() ->
    [{doc, "Tests argument consumption option, with nargs"}].

args(Config) when is_list(Config) ->
    %% consume optional list arguments
    Opts = [
        #{name => arg, short => $s, nargs => list, type => int},
        #{name => bool, short => $b, type => boolean}
    ],
    ?assertEqual(#{arg => [1, 2, 3], bool => true},
        parse_opts(["-s 1 2 3 -b"], Opts)),
    %% consume one_or_more arguments in an optional list
    Opts2 = [
        #{name => arg, short => $s, nargs => nonempty_list},
        #{name => extra, short => $x}
        ],
    ?assertEqual(#{extra => "X", arg => ["a","b","c"]},
        parse_opts(["-s port -s a b c -x X"], Opts2)),
    %% error if there is no argument to consume
    ?assertException(error, {args, {invalid_argument, _, arg, ["-x"]}},
        parse_opts(["-s -x"], Opts2)),
    %% error when positional has nargs = nonempty_list or pos_integer
    ?assertException(error, {args, {missing_argument, _, req}},
        parse_opts([""], [#{name => req, nargs => nonempty_list}])),
    %% positional arguments consumption: one or more positional argument
    OptsPos1 = [
        #{name => arg, nargs => nonempty_list},
        #{name => extra, short => $x}
    ],
    ?assertEqual(#{extra => "X", arg => ["b","c"]},
        parse_opts(["-x port -x a b c -x X"], OptsPos1)),
    %% positional arguments consumption, any number (maybe zero)
    OptsPos2 = #{arguments => [
        #{name => arg, nargs => list},
        #{name => extra, short => $x}
    ]},
    ?assertEqual(#{extra => "X", arg => ["a","b","c"]}, parse(["-x port a b c -x X"], OptsPos2)),
    %% positional: consume ALL arguments!
    OptsAll = [
        #{name => arg, nargs => all},
        #{name => extra, short => $x}
    ],
    ?assertEqual(#{extra => "port", arg => ["a","b","c", "-x", "X"]},
        parse_opts(["-x port a b c -x X"], OptsAll)),
    %%
    OptMaybe = [
        #{name => foo, long => "-foo", nargs => {'maybe', c}, default => d},
        #{name => bar, nargs => 'maybe', default => d}
    ],
    ?assertEqual(#{foo => "YY", bar => "XX"},
        parse_opts(["XX --foo YY"], OptMaybe)),
    ?assertEqual(#{foo => c, bar => "XX"},
        parse_opts(["XX --foo"], OptMaybe)),
    ?assertEqual(#{foo => d, bar => d},
        parse_opts([""], OptMaybe)),
    %% maybe with default
    ?assertEqual(#{foo => d, bar => "XX", baz => ok},
        parse_opts(["XX -b"], [#{name => baz, nargs => 'maybe', short => $b, default => ok} | OptMaybe])),
    %% maybe arg - with no default given
    ?assertEqual(#{foo => d, bar => "XX", baz => 0},
        parse_opts(["XX -b"], [#{name => baz, nargs => 'maybe', short => $b, type => int} | OptMaybe])),
    ?assertEqual(#{foo => d, bar => "XX", baz => ""},
        parse_opts(["XX -b"], [#{name => baz, nargs => 'maybe', short => $b, type => string} | OptMaybe])),
    ?assertEqual(#{foo => d, bar => "XX", baz => undefined},
        parse_opts(["XX -b"], [#{name => baz, nargs => 'maybe', short => $b, type => atom} | OptMaybe])),
    ?assertEqual(#{foo => d, bar => "XX", baz => <<"">>},
        parse_opts(["XX -b"], [#{name => baz, nargs => 'maybe', short => $b, type => binary} | OptMaybe])),
    %% nargs: optional list, yet it still needs to be 'not required'!
    OptList = [#{name => arg, nargs => list, required => false, type => int}],
    ?assertEqual(#{}, parse_opts("", OptList)),
    ok.

argparse() ->
    [{doc, "Tests examples from argparse Python library"}].

argparse(Config) when is_list(Config) ->
    Parser = #{arguments => [
        #{name => sum, long => "-sum", action => {store, sum}, default => max},
        #{name => integers, type => int, nargs => nonempty_list}
        ]},
    ?assertEqual(#{integers => [1, 2, 3, 4], sum => max}, parse("1 2 3 4", Parser)),
    ?assertEqual(#{integers => [1, 2, 3, 4], sum => sum}, parse("1 2 3 4 --sum", Parser)),
    ?assertEqual(#{integers => [7, -1, 42], sum => sum}, parse("--sum 7 -1 42", Parser)),
    %% name or flags
    Parser2 = #{arguments => [
        #{name => bar, required => true},
        #{name => foo, short => $f, long => "-foo"}
    ]},
    ?assertEqual(#{bar => "BAR"}, parse("BAR", Parser2)),
    ?assertEqual(#{bar => "BAR", foo => "FOO"}, parse("BAR --foo FOO", Parser2)),
    %PROG: error: the following arguments are required: bar
    ?assertException(error, {args, {missing_argument, _, bar}}, parse("--foo FOO", Parser2)),
    %% action tests: default
    ?assertEqual(#{foo => "1"},
        parse("--foo 1", #{arguments => [#{name => foo, long => "-foo"}]})),
    %% action test: store
    ?assertEqual(#{foo => 42},
        parse("--foo", #{arguments => [#{name => foo, long => "-foo", action => {store, 42}}]})),
    %% action tests: boolean (variants)
    ?assertEqual(#{foo => true},
        parse("--foo", #{arguments => [#{name => foo, long => "-foo", action => {store, true}}]})),
    ?assertEqual(#{foo => true},
        parse("--foo", #{arguments => [#{name => foo, long => "-foo", type => boolean}]})),
    ?assertEqual(#{foo => true},
        parse("--foo true", #{arguments => [#{name => foo, long => "-foo", type => boolean}]})),
    ?assertEqual(#{foo => false},
        parse("--foo false", #{arguments => [#{name => foo, long => "-foo", type => boolean}]})),
    %% action tests: append & append_const
    ?assertEqual(#{all => [1, "1"]},
        parse("--x 1 -x 1", #{arguments => [
            #{name => all, long => "-x", type => int, action => append},
            #{name => all, short => $x, action => append}]})),
    ?assertEqual(#{all => ["Z", 2]},
        parse("--x -x", #{arguments => [
            #{name => all, long => "-x", action => {append, "Z"}},
            #{name => all, short => $x, action => {append, 2}}]})),
    %% count:
    ?assertEqual(#{v => 3},
        parse("-v -v -v", #{arguments => [#{name => v, short => $v, action => count}]})),
    ok.

negative() ->
    [{doc, "Test negative number parser"}].

negative(Config) when is_list(Config) ->
    Parser = #{arguments => [
        #{name => x, short => $x, type => int, action => store},
        #{name => foo, nargs => 'maybe', required => false}
    ]},
    ?assertEqual(#{x => -1}, parse("-x -1", Parser)),
    ?assertEqual(#{x => -1, foo => "-5"}, parse("-x -1 -5", Parser)),
    %%
    Parser2 = #{arguments => [
        #{name => one, short => $1},
        #{name => foo, nargs => 'maybe', required => false}
    ]},

    %% negative number options present, so -1 is an option
    ?assertEqual(#{one => "X"}, parse("-1 X", Parser2)),
    %% negative number options present, so -2 is an option
    ?assertException(error, {args, {unknown_argument, _, "-2"}}, parse("-2", Parser2)),

    %% negative number options present, so both -1s are options
    ?assertException(error, {args, {missing_argument,_,one}}, parse("-1 -1", Parser2)),
    %% no "-" prefix, can only be an integer
    ?assertEqual(#{foo => "-1"}, args:parse(["-1"], Parser2, #{prefixes => "+"})),
    %% no "-" prefix, can only be an integer, but just one integer!
    ?assertException(error, {args, {unknown_argument, _, "-1"}},
        args:parse(["-2", "-1"], Parser2, #{prefixes => "+"})),
    %% just in case, floats work that way too...
    ?assertException(error, {args, {unknown_argument, _, "-2"}},
        parse("-2", #{arguments => [#{name => one, long => "1.2"}]})).

nodigits() ->
    [{doc, "Test prefixes and negative numbers together"}].

nodigits(Config) when is_list(Config) ->
    %% verify nodigits working as expected
    Parser3 = #{arguments => [
        #{name => extra, short => $3},
        #{name => arg, nargs => list}
    ]},
    %% ensure not to consume optional prefix
    ?assertEqual(#{extra => "X", arg => ["a","b","3"]},
        args:parse(string:lexemes("-3 port a b 3 +3 X", " "), Parser3, #{prefixes => "-+"})).
    %% verify split_to_option working with weird prefix
    %?assertEqual(#{extra => "X", arg => ["a","b","-3"]},
    %    args:parse(string:lexemes("-3 port a b -3 -3 X", " "), Parser3, #{prefixes => "-+"})).

python_issue_15112() ->
    [{doc, "Tests for https://bugs.python.org/issue15112"}].

python_issue_15112(Config) when is_list(Config) ->
    Parser = #{arguments => [
        #{name => pos},
        #{name => foo},
        #{name => spam, default => 24, type => int, long => "-spam"},
        #{name => vars, nargs => list}
    ]},
    ?assertEqual(#{pos => "1", foo => "2", spam => 8, vars => ["8", "9"]},
        parse("1 2 --spam 8 8 9", Parser)).

default_for_not_required() ->
    [{doc, "Tests that default value is used for non-required positional argument"}].

default_for_not_required(Config) when is_list(Config) ->
    ?assertEqual(#{def => 1}, parse("", #{arguments => [#{name => def, short => $d, required => false, default => 1}]})),
    ?assertEqual(#{def => 1}, parse("", #{arguments => [#{name => def, required => false, default => 1}]})).

global_default() ->
    [{doc, "Tests that a global default can be enabled for all non-required arguments"}].

global_default(Config) when is_list(Config) ->
    ?assertEqual(#{def => "global"}, args:parse("", #{arguments => [#{name => def, type => int, required => false}]},
        #{default => "global"})).

subcommand() ->
    [{doc, "Tests subcommands parser"}].

subcommand(Config) when is_list(Config) ->
    TwoCmd = #{arguments => [#{name => bar}]},
    Cmd = #{
        arguments => [#{name => force, type => boolean, short => $f}],
        commands => #{"one" => #{
            arguments => [#{name => foo, type => boolean, long => "-foo"}, #{name => baz}],
            commands => #{
                "two" => TwoCmd}}}},
    ?assertEqual(
        {#{force => true, baz => "N1O1O", foo => true, bar => "bar"}, {["one", "two"], TwoCmd}},
        parse("one N1O1O -f two --foo bar", Cmd)),
    %% it is an error not to choose subcommand
    ?assertException(error, {args, {missing_argument,_,"missing handler"}},
        parse("one N1O1O -f", Cmd)).

error_format() ->
    [{doc, "Tests error output formatter"}].

error_format(Config) when is_list(Config) ->
    %% does not really require testing, but serve well as contract,
    %%  and good for coverage
    {ok, [[Prog]]} = init:get_argument(progname),
    ?assertEqual(Prog ++ ": internal error, invalid field 'commands': sub-commands must be a map\n",
        make_error([""], #{commands => []})),
    ?assertEqual(Prog ++ " one: internal error, invalid field 'commands': sub-commands must be a map\n",
        make_error([""], #{commands => #{"one" => #{commands => []}}})),
    ?assertEqual(Prog ++ " one two: internal error, invalid field 'commands': sub-commands must be a map\n",
        make_error([""], #{commands => #{"one" => #{commands => #{"two" => #{commands => []}}}}})),
        %%
    ?assertEqual(Prog ++ ": internal error, option  field 'name': argument must be a map, and specify 'name'\n",
        make_error([""], #{arguments => [#{}]})),
    %%
    ?assertEqual(Prog ++ ": internal error, option name field 'type': unsupported\n",
        make_error([""], #{arguments => [#{name => name, type => foo}]})),
    ?assertEqual(Prog ++ ": internal error, option name field 'nargs': unsupported\n",
        make_error([""], #{arguments => [#{name => name, nargs => foo}]})),
    ?assertEqual(Prog ++ ": internal error, option name field 'action': unsupported\n",
        make_error([""], #{arguments => [#{name => name, action => foo}]})),
    %% unknown arguments
    ?assertEqual(Prog ++ ": unrecognised argument: arg\n", make_error(["arg"], #{})),
    ?assertEqual(Prog ++ ": unrecognised argument: -a\n", make_error(["-a"], #{})),
    %% missing argument
    ?assertEqual(Prog ++ ": required argument missing: need\n", make_error([""],
        #{arguments => [#{name => need}]})),
    ?assertEqual(Prog ++ ": required argument missing: need\n", make_error([""],
        #{arguments => [#{name => need, short => $n, required => true}]})),
    %% invalid value
    ?assertEqual(Prog ++ ": invalid argument foo for: need\n", make_error(["foo"],
        #{arguments => [#{name => need, type => int}]})),
    ?assertEqual(Prog ++ ": invalid argument cAnNotExIsT for: need\n", make_error(["cAnNotExIsT"],
        #{arguments => [#{name => need, type => atom}]})),
    ok.

very_short() ->
    [{doc, "Tests short option appended to the optional itself"}].

very_short(Config) when is_list(Config) ->
    ?assertEqual(#{x => "V"},
        parse("-xV", #{arguments => [#{name => x, short => $x}]})).

multi_short() ->
    [{doc, "Tests multiple short arguments blend into one"}].

multi_short(Config) when is_list(Config) ->
    %% ensure non-flammable argument does not explode, even when it's possible
    ?assertEqual(#{v => "xv"},
        parse("-vxv", #{arguments => [#{name => v, short => $v}, #{name => x, short => $x}]})),
    %% ensure 'verbosity' use-case works
    ?assertEqual(#{v => 3},
        parse("-vvv", #{arguments => [#{name => v, short => $v, action => count}]})),
    %%
    ?assertEqual(#{recursive => true, force => true, path => "dir"},
        parse("-rf dir", #{arguments => [
            #{name => recursive, short => $r, type => boolean},
            #{name => force, short => $f, type => boolean},
            #{name => path}
            ]})).

proxy_arguments() ->
    [{doc, "Tests nargs => all used to proxy arguments to another script"}].

proxy_arguments(Config) when is_list(Config) ->
    Cmd = #{
        commands => #{
            "start" => #{
                arguments => [
                    #{name => shell, short => $s, long => "-shell", type => boolean},
                    #{name => skip, short => $x, long => "-skip", type => boolean},
                    #{name => args, required => false, nargs => all}
                ]
            },
            "stop" => #{},
            "status" => #{
                arguments => [
                    #{name => skip, required => false, default => "ok"},
                    #{name => args, required => false, nargs => all}
                ]},
            "state" => #{
                arguments => [
                    #{name => skip, required => false},
                    #{name => args, required => false, nargs => all}
                ]}
        },
        arguments => [
            #{name => node}
        ],
        handler => fun (#{}) -> ok end
    },
    ?assertEqual(#{node => "node1"}, parse("node1", Cmd)),
    ?assertEqual({#{node => "node1"}, {["stop"], #{}}}, parse("node1 stop", Cmd)),
    ?assertMatch({#{node := "node2.org", shell := true, skip := true}, _}, parse("node2.org start -x -s", Cmd)),
    ?assertMatch({#{args := ["-app","key","value"],node := "node1.org"}, {["start"], _}},
        parse("node1.org start -app key value", Cmd)),
    ?assertMatch({#{args := ["-app","key","value", "-app2", "key2", "value2"],node := "node3.org", shell := true}, {["start"], _}},
        parse("node3.org start -s -app key value -app2 key2 value2", Cmd)),
    %% test that any non-required positionals are skipped
    ?assertMatch({#{args := ["-a","bcd"], node := "node2.org", skip := "ok"}, _}, parse("node2.org status -a bcd", Cmd)),
    ?assertMatch({#{args := ["-app", "key"], node := "node2.org"}, _}, parse("node2.org state -app key", Cmd)).


usage() ->
    [{doc, "Basic tests for help formatter, including 'hidden' help"}].

usage(Config) when is_list(Config) ->
    Cmd = ubiq_cmd(),
    Usage = "usage: " ++ prog() ++ " start {crawler|doze} [-lrfv] [-s <shard>...] [-z <z>] [-m <more>] [-b <bin>] [-g <g>] [-t <t>] ---maybe-req -y <y>"
        " --yyy <y> [-u <u>] [-c <choice>] [-q <fc>] [-w <ac>] [--unsafe <au>] [--safe <as>] [-foobar <long>] [--force] [-i <interval>] [--req <weird>] [--float <float>] <server> [<optpos>]"
        "\n\nSubcommands:\n  crawler      controls crawler behaviour\n  doze         dozes a bit\n\nArguments:\n  server       server to start\n  optpos       optional positional (int)"
        "\n\nOptional arguments:\n  -s           initial shards (int)\n  -z           between (1 <= int <= 10)\n  -l           maybe lower (int <= 10)"
        "\n  -m           less than 10 (int <= 10)\n  -b           binary with re (binary re: m)\n  -g           binary with re (binary re: m)\n  -t           string with re (string re: m)"
        "\n  ---maybe-req maybe required int (int)\n  -y, --yyy    string with re (string re: m)\n  -u           string choices (choice: 1, 2)\n  -c           tough choice (choice: 1, 2, 3)"
        "\n  -q           floating choice (choice: 2.10000, 1.20000)\n  -w           atom choice (choice: one, two)\n  --unsafe     unsafe atom (atom)\n  --safe       safe atom (existing atom)"
        "\n  -foobar      foobaring option\n  -r           recursive\n  -f, --force  force\n  -v           verbosity level"
        "\n  -i           interval set (int >= 1)\n  --req        required optional, right?\n  --float      floating-point long form argument (float, 3.14)\n",
    ?assertEqual(Usage, args:help(Cmd, #{command => ["start"]})),
    FullCmd = "usage: " ++ prog() ++ " <command> [-rfv] [--force] [-i <interval>] [--req <weird>] [--float <float>]\n\nSubcommands:\n  start       verifies configuration and starts server"
        "\n  status      prints server status\n  stop        stops running server\n\nOptional arguments:\n  -r          recursive\n  -f, --force force"
        "\n  -v          verbosity level\n  -i          interval set (int >= 1)\n  --req       required optional, right?\n  --float     floating-point long form argument (float, 3.14)\n",
    ?assertEqual(FullCmd, args:help(Cmd)),
    CrawlerStatus = "usage: " ++ prog() ++ " status crawler [-rfv] [---extra <extra>] [--force] [-i <interval>] [--req <weird>] [--float <float>]\n\nOptional arguments:\n"
        "  ---extra    extra option very deep\n  -r          recursive\n  -f, --force force\n  -v          verbosity level"
        "\n  -i          interval set (int >= 1)\n  --req       required optional, right?\n  --float     floating-point long form argument (float, 3.14)\n",
    ?assertEqual(CrawlerStatus, args:help(Cmd, #{command => ["status", "crawler"]})),
    ok.

usage_required_args() ->
    [{doc, "Verify that required args are printed as required in usage"}].

usage_required_args(Config) when is_list(Config) ->
    Cmd = #{commands => #{"test" => #{arguments => [#{name => required, required => true, long => "-req"}]}}},
    Expected = "",
    ?assertEqual(Expected, args:help(Cmd, #{command => ["test"]})).

error_usage() ->
    [{doc, "Test that usage information is added to errors"}].

%% This test does not verify usage printed,
%%  but at least ensures formatter does not crash.
error_usage(Config) when is_list(Config) ->
    try parse("start -rf", ubiq_cmd())
    catch error:{args, Reason} ->
        Actual = args:format_error(Reason, ubiq_cmd(), #{}),
        ct:pal("error: ~s", [Actual])
    end,
    ok.

meta() ->
    [{doc, "Tests found while performing meta-testing"}].

%% This test does not verify usage printed,
%%  but at least ensures formatter does not crash.
meta(Config) when is_list(Config) ->
    %% short option with no argument, when it's needed
    ?assertException(error, {args, {missing_argument, _, short49}},
        parse("-1", #{arguments => [#{name => short49, short => 49}]})),
    %% extend + maybe
    ?assertException(error, {args, {invalid_option, _, short49, action, _}},
        parse("-1 -1", #{arguments =>
        [#{action => extend, name => short49, nargs => 'maybe', short => 49}]})),
    %%
    ?assertEqual(#{short49 => 2},
        parse("-1 arg1 --force", #{arguments =>
        [#{action => count, long => "-force", name => short49, nargs => 'maybe', short => 49}]})),
    ok.

usage_template() ->
    [{doc, "Tests templates in help/usage"}].

usage_template(Config) when is_list(Config) ->
    %% Argument (positional)
    Cmd = #{arguments => [#{
        name => shard,
        type => int,
        default => 0,
        help => {"[-s SHARD]", ["initial number, ", type, " with a default value of ", default]}}
    ]},
    ?assertEqual("usage: " ++ prog() ++ " [-s SHARD]\n\nArguments:\n  shard initial number, int with a default value of 0\n",
        args:help(Cmd, #{})),
    %% Optional
    Cmd1 = #{arguments => [#{
        name => shard,
        short => $s,
        type => int,
        default => 0,
        help => {"[-s SHARD]", ["initial number"]}}
    ]},
    ?assertEqual("usage: " ++ prog() ++ " [-s SHARD]\n\nOptional arguments:\n  -s initial number\n",
        args:help(Cmd1, #{})),
    %% ISO Date example
    DefaultRange = {{2020, 1, 1}, {2020, 6, 22}},
    CmdISO = #{
        arguments => [
            #{
                name => range,
                long => "-range",
                short => $r,
                help => {"[--range RNG]", fun() ->
                    {{FY, FM, FD}, {TY, TM, TD}} = DefaultRange,
                    lists:flatten(io_lib:format("date range, ~b-~b-~b..~b-~b-~b", [FY, FM, FD, TY, TM, TD]))
                                              end},
                type => {custom, fun(S) -> [S, DefaultRange] end},
                default => DefaultRange
            }
        ]
    },
    ?assertEqual("usage: " ++ prog() ++ " [--range RNG]\n\nOptional arguments:\n  -r, --range date range, 2020-1-1..2020-6-22\n",
        args:help(CmdISO, #{})),
    ok.
