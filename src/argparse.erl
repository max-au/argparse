%%%-------------------------------------------------------------------
%%% @author Maxim Fedorov
%%% @copyright (C) 2019, Maxim Fedorov
%%% @doc
%%% Command line parser, made with hierarchy of commands in mind.
%%%
%%% Inspired by Python argparse library.
%%% Known incompatibilities:
%%%  * boolean type support, that does not require {store, true},
%%%     yet still consuming true/false argument from command line,
%%%     if it is there
%%%
%%% TODO: usage/help
%%% TODO: add abbreviated support (shortened long forms)
%%% TODO: add support for multiple short options, e.g. -vvv, -rf
%%% TODO: 'choices' validation
%%% TODO: add shell auto-complete
%%% Explore: support "--arg=value" form
%%% Explore: support "-aValue" form
%%% Explore: support "--no-XXXX" form for booleans
%%%
%%% @end
%%%

-module(argparse).
-author("maximfca@gmail.com").

-export([
    validate/1,
    parse/2,
    format_error/1
]).

%%--------------------------------------------------------------------
%% API

%% Built-in types include basic validation abilities
%% May allow to create new atoms (not the default)
%% Custom conversion is also allowed.
-type arg_type() :: boolean |
    float | {float, [{min, float()} | {max, float()}]} |
    int | {int, [{min, integer()} | {max, integer()}]} |
    string | {string, string()} | {string, string(), [term()]} |
    binary | {binary, binary()} | {binary, binary(), [term()]} |
    atom | {atom, create} |
    {custom, fun((string()) -> term())}.

%% Command line argument specification.
%% Argument can be optional - starting with - (dash), and positional.
-type argument() :: #{
    %% Argument name, and a destination to store value too
    %% It is allowed to have several arguments named the same, setting or appending to the same variable
    name := atom(),

    %% short, single-character variant of command line option, omitting dash (example: $b, meaning -b),
    %%  when set, this is optional argument
    short => char(),

    %% long command line option, omitting first dash (example: "kernel", or "-long", meaning "-kernel" and "--long"
    %% long command always wins over short abbreviation (e.g. -kernel is considered before -k -e -r -n -e -l)
    %%  when set, this is optional argument
    long => string(),

    %% produces an error if option was not present in command line
    required => true,

    %% default value, attached if it's not found in a command line
    default => term(),

    %% parameter type (string by default)
    type => arg_type(),

    %% action to take when argument is matched
    action => store | {store, term()} | append | {append, term()} | count | extend,

    %% how many positional arguments to consume when option is matched
    nargs =>
        pos_integer() | %% consume exactly this amount, e.g. '-kernel key value' #{long => "-kernel", args => 2}
                        %%  returns #{kernel => ["key", "value"]}
        maybe |         %% same as '?' with no const defined
        {maybe, term()} |   %% same as '?' with const = term()
        list |          %% "*"
        nonempty_list | %% "+"
        all,            %% remainder

    %% help string printed in usage
    help => string()
}.

%% Arguments map: argument name to a term, produced by parser.
%% Supplied to command handler
-type arg_map() :: #{atom() => term()}.

%% Command handler. May produce some output!
-type handler() :: fun((arg_map()) -> term()).

%% Custom function to merge output returned by multi-destination command.
-type output_merge_fun() :: fun (([term()]) -> term()).

%% Command line call specification. May contain sub-commands and/or options.
%% Match algorithm:
%%  - if it is optional argument (starting with -), look for all available
%%      optional argument descriptions in the command stack, error if not found,
%%      and optional argument starting with single '-' cannot be converted to
%%      a negative number (in this case it is assumed to be positional argument)
%%  - positional argument: try to match sub-command name,  otherwise
%%      treat as positional argument
%%
%% Sub-commands are arranged into maps.
%% Command with empty atom name '' serves as catch-all command.
-type parser_map() :: #{atom() => parser()}.

-type parser() :: #{
    %% Sub-parsers
    commands => parser_map(),

    %% Custom handler function. Default is Mod:Name where Mod is
    %%  module exporting ectl beaviour, and Name is command name
    handler => handler(),

    %% Remote call handling, global/scoped execution
    % Local is 'extra remote calls not allowed',
    %   global is 'use default result merge behaviour',
    %   otherwise merge() fun is supplied that should
    %   implement custom response merging logic
    %% NOT IMPLEMENTED - add it for your own convenience
    global => local | global | output_merge_fun(),

    %% help line reported by usage()
    help => string(),

    %% accepted options list. Positional order is important!
    options => [argument()]
}.

-type cmd_path() :: [{atom(), parser()}].

-export_type([
    argument/0,
    parser/0,
    handler/0,
    cmd_path/0,
    arg_map/0
]).

%% Optional or positional argument?
-define(IS_OPTIONAL(Arg), is_map_key(short, Arg) orelse is_map_key(long, Arg)).
-define(IS_POSITIONAL(Arg), not (is_map_key(short, Arg) orelse is_map_key(long, Arg))).

%% Parser state (not available via API)
-record(eos, {
    %% matched arguments map
    argmap = #{} :: arg_map(),
    %% collected command path
    path :: undefined | cmd_path(),
    %% unmatched positional arguments encountered on the path
    pos = [] :: [argument()],
    %% all optional arguments encountered on the path
    opt = [] :: [argument()]
}).

%% Error Reason thrown by parser (feed it into format_error to get human-readable error).
-type error() ::
    {invalid_command, cmd_path(), Field :: atom(), Reason :: string()} |
    {invalid_option, cmd_path(), argument(), Field :: atom(), Reason :: string()} |
    {unknown_option, cmd_path(), Argument :: string()} |
    {missing_argument, cmd_path(), argument()} |
    {invalid_argument, cmd_path(), argument(), Argument :: string()}.

%% @doc
%% Validates command specification, throws if there is an error, and
%%  returns canonical specification.
-spec validate(parser()) -> parser().
validate(Command) ->
    validate_command([{undefined, Command}]).

%% @doc
%% Parses supplied arguments according to expected command definition.
%% Returns reversed list of commands parsed.
-spec parse(Args :: [string()], parser()) -> {cmd_path() | undefined, arg_map()}.
parse(Args, Command) ->
    parse_impl(Args, validate(Command), add_opts(maps:get(options, Command, []), #eos{})).

%% Format exceptions produced by parse/2
-spec format_error(error()) -> string().
format_error({invalid_command, Path, Text}) ->
    lists:flatten(io_lib:format("~p invalid command (~s)", [Path, Text]));
format_error({invalid_option, Path, Text, Option}) ->
    lists:flatten(io_lib:format("~p invalid option ~p (~s)", [Path, Option, Text]));
format_error({unknown_option, Path, Argument}) ->
    lists:flatten(io_lib:format("~p unknown option ~s", [Path, Argument]));
format_error({missing_argument, Path, Option}) ->
    lists:flatten(io_lib:format("~p missing option ~s", [Path, Option]));
format_error({invalid_argument, Path, Option, Value}) ->
    lists:flatten(io_lib:format("~p invalid option ~s (~p)", [Path, Option, Value])).

%%--------------------------------------------------------------------
%% Internal implementation

%% Option starting with "-"
parse_impl([[$- | Optname] = Optional | Tail], Current, #eos{opt = Opt} = Eos) ->
    case find_option(Optname, Opt) of
        {short, OptVal} ->
            ct:pal("Short optional ~s when ~p (~200p)", [Optional, Current, Eos]),
            consume(Tail, Current, OptVal, Eos);
        %% here will be support for short-abbreviated options
        {long, OptVal} ->
            ct:pal("Long optional ~s when ~p (~200p)", [Optional, Current, Eos]),
            consume(Tail, Current, OptVal, Eos);
        not_found ->
            %% what if it's an integer, or float value?
            case is_digits(Optional) andalso no_digits(Opt) of
                true ->
                    ct:pal("Positional negative ~s when ~p (~200p)", [Optional, Current, Eos]),
                    parse_positional(Optional, Tail, Current, Eos);
                false ->
                    error({unknown_option, Eos#eos.path, Optional})
            end
    end;

%% Options not starting with "-": can be a sub-command, or a positional
%%  argument.
%% Next clause is for commands with sub-commands.
parse_impl([Positional | Tail], #{commands := CmdMap} = Current, Eos) ->
    ct:pal("(Command?) ~s when ~p (~200p)", [Positional, Current, Eos]),
    try
        %% command names are always atoms, so if it cannot be converted,
        %%  it is not a command
        ArgName = list_to_existing_atom(Positional),
        case maps:find(ArgName, CmdMap) of
            error ->
                %% sub-command not found, try positional argument
                parse_positional(Positional, Tail, Current, Eos);
            {ok, #{options := AddOpt} = SubCmd} ->
                %% found matching sub-command, descend into it
                parse_impl(Tail, SubCmd, add_opts(AddOpt, add_cmd(ArgName, SubCmd, Eos)));
            {ok, SubCmd} ->
                %% found matching sub-command with no options, descend into it
                parse_impl(Tail, SubCmd, add_cmd(ArgName, SubCmd, Eos))
        end
    catch
        error:badarg ->
            %% atom conversion failed, it can never be a sub-command
            parse_positional(Positional, Tail, CmdMap, Eos)
    end;

%% Clause for options that don't have sub-commands (hence, it must be
%%  positional argument).
parse_impl([Positional | Tail], Current, Eos) ->
    ct:pal("Positional (no commands) ~s when ~120p (~200p)", [Positional, Current, Eos]),
    parse_positional(Positional, Tail, Current, Eos);

%% no more arguments left, go verify no required option is missing, and set up default
%%  values for those aren't provided
parse_impl([], Current, #eos{path = Path, argmap = ArgMap, opt = Opts, pos = Pos}) ->
    %% error out for any 'required' argument
    %% by default, all positional arguments are required, unless they
    %%  have required => false, or nargs set
    [error({missing_argument, Path, OptName}) ||
        Opt = #{name := OptName} <- Opts ++ Pos,
        is_map_key(OptName, ArgMap) =:= false,      %% argument is not present in the output map
        (maps:get(required, Opt, false) =:= true    %% required explicitly set to true
            orelse (not is_map_key(nargs, Opt) andalso ?IS_POSITIONAL(Opt)))  %% or not set for positional that does not have nargs
    ],
    %% blend in default values, when known, for all items in the Path
    {Path, blend_default(Path, Current, ArgMap)}.

blend_default(undefined, Current, ArgMap) ->
    blend_default([Current], undefined, ArgMap);
blend_default(Path, _Current, ArgMap) ->
    lists:foldl(
        fun (#{options := DefOpts}, Acc) ->
            lists:foldl(
                fun (#{name := Name, default := Def}, AccIn)
                    when not is_map_key(Name, AccIn) ->
                    AccIn#{Name => Def};
                    (_, AccIn) ->
                        AccIn
                end, Acc, DefOpts);
            (_, Acc) ->
                Acc
        end, ArgMap, Path).

%% adds subcommand to path
add_cmd(CmdName, CmdSpec, #eos{path = undefined} = Eos) ->
    Eos#eos{path = [{CmdName, CmdSpec}]};
add_cmd(CmdName, CmdSpec, #eos{path = Path} = Eos) ->
    Eos#eos{path = [{CmdName, CmdSpec} | Path]}.

%% adds options into current set of discovered options, in a reverse
%%  order for optional, and forward order for positional (as it's important)
add_opts([], Eos) ->
    Eos;
add_opts([Option | Tail], #eos{opt = Opt} = Eos) when is_map_key(short, Option); is_map_key(long, Option) ->
    add_opts(Tail, Eos#eos{opt = [Option | Opt]});
add_opts([PosOpt | Tail], #eos{pos = Pos} = Eos) ->
    add_opts(Tail, Eos#eos{pos = Pos ++ [PosOpt]}).

%% Finds spec that matches optional argument passed in
find_option(_Arg, []) ->
    not_found;
%% shortcut for long option first
find_option(Arg, [Opt | _Tail]) when map_get(long, Opt) =:= Arg ->
    {long, Opt};
find_option([Short | _], [Opt | _Tail]) when map_get(short, Opt) =:= Short ->
    {short, Opt};
find_option(Arg, [_ | Tail]) ->
    find_option(Arg, Tail).

%%--------------------------------------------------------------------
%% positional argument for all commands in the stack

parse_positional(Arg, _Tail, _Current, #eos{pos = [], path = Path}) ->
    %% no match for positional argument supplied, and no sub-command either
    error({unknown_option, Path, Arg});
parse_positional(Arg, Tail, Current, #eos{pos = Pos} = Eos) ->
    %% positional argument itself is a value
    consume([Arg | Tail], Current, hd(Pos), Eos).

%%--------------------------------------------------------------------
%% argument consumption (nargs) handling

%% consume predefined amount (none of which can be an option?)
consume(Tail, Current, #{nargs := Count} = Opt, Eos) when is_integer(Count) ->
    ct:pal("~p - nargs into ~p (~200p)", [Tail, Opt, Eos]),
    {Consumed, Remain} = split_to_option(Tail, Count, Eos#eos.opt, []),
    length(Consumed) < Count andalso error({invalid_argument, Eos#eos.path, maps:get(name, Opt), Tail}),
    action(Remain, Current, Consumed, Opt#{type => {list, maps:get(type, Opt, string)}}, Eos);

%% handle 'reminder' by just dumping everything in
consume(Tail, Current, #{nargs := all} = Opt, Eos) ->
    ct:pal("Consuming all into ~p (~200p)", [Opt, Eos]),
    action([], Current, Tail, Opt#{type => {list, maps:get(type, Opt, string)}}, Eos);

%% require at least one argument
consume(Tail, Current, #{nargs := nonempty_list} = Opt, Eos) ->
    ct:pal("~p - consuming non-empty list into ~p (~p200)", [Tail, Opt, Eos]),
    {Consumed, Remains} = split_to_option(Tail, -1, Eos#eos.opt, []),
    Consumed =:= [] andalso error({invalid_argument, Eos#eos.path, maps:get(name, Opt), Tail}),
    action(Remains, Current, Consumed, Opt#{type => {list, maps:get(type, Opt, string)}}, Eos);

%% consume all until next option
consume(Tail, Current, #{nargs := list} = Opt, Eos) ->
    ct:pal("~p - consuming list into ~p (~200p)", [Tail, Opt, Eos]),
    {Consumed, Remains} = split_to_option(Tail, -1, Eos#eos.opt, []),
    action(Remains, Current, Consumed, Opt#{type => {list, maps:get(type, Opt, string)}}, Eos);

%% maybe consume one, maybe not...
%% special cases for 'boolean maybe', only consume 'true' and 'false'
consume(["true" | Tail], Current, #{type := boolean} = Opt, Eos) ->
    action(Tail, Current, true, Opt#{type => raw}, Eos);
consume(["false" | Tail], Current, #{type := boolean} = Opt, Eos) ->
    action(Tail, Current, false, Opt#{type => raw}, Eos);
consume(Tail, Current, #{type := boolean} = Opt, Eos) ->
    %% if neither true or false, don't consume, just do the action with 'true' as arg
    action(Tail, Current, true, Opt#{type => raw}, Eos);

%% maybe behaviour, as '?'
consume(Tail, Current, #{nargs := maybe} = Opt, Eos) ->
    ct:pal("~p - consuming maybe into ~p (~200p)", [Tail, Opt, Eos]),
    case split_to_option(Tail, 1, Eos#eos.opt, []) of
        {[], _} ->
            %% no argument given, produce default argument (if not present,
            %%  then produce default value of the specified type)
            action(Tail, Current, default(Opt), Opt#{type => raw}, Eos);
        {[Consumed], Remains} ->
            action(Remains, Current, Consumed, Opt, Eos)
    end;

%% maybe consume one, maybe not...
consume(Tail, Current, #{nargs := {maybe, Const}} = Opt, Eos) ->
    ct:pal("~p - const maybe into ~p (~200p)", [Tail, Opt, Eos]),
    case split_to_option(Tail, 1, Eos#eos.opt, []) of
        {[], _} ->
            action(Tail, Current, Const, Opt, Eos);
        {[Consumed], Remains} ->
            action(Remains, Current, Consumed, Opt, Eos)
    end;

%% default case, which depends on action
consume(Tail, Current, #{action := count} = Opt, Eos) ->
    ct:pal("~p - counting to ~p (~200p)", [Tail, Opt, Eos]),
    action(Tail, Current, undefined, Opt, Eos);

%% for {store, ...} and {append, ...} don't take argument out
consume(Tail, Current, #{action := {Act, _Const}} = Opt, Eos) when Act =:= store; Act =:= append ->
    ct:pal("~p - action ~s from ~p (~200p)", [Tail, Act, Opt, Eos]),
    action(Tail, Current, undefined, Opt, Eos);

%% optional: ensure not to consume another option start
consume([[$- | _] =ArgValue | Tail], Current, Opt, Eos) when ?IS_OPTIONAL(Opt) ->
    ct:pal("~p - looking for ~s to go into ~p (~200p)", [Tail, ArgValue, Opt, Eos]),
    case is_digits(ArgValue) andalso no_digits(Eos#eos.opt) of
        true ->
            action(Tail, Current, ArgValue, Opt, Eos);
        false ->
            error({missing_argument, Eos#eos.path, maps:get(name, Opt)})
    end;

consume([ArgValue | Tail], Current, Opt, Eos) when ?IS_OPTIONAL(Opt) ->
    ct:pal("~p - optional ~s into ~p (~200p)", [Tail, ArgValue, Opt, Eos]),
    action(Tail, Current, ArgValue, Opt, Eos);

%% positional: just consume one, no 'maybe' behaviour
consume([ArgValue | Tail], Current, Opt, Eos) ->
    ct:pal("~p - positional ~s into ~p (~200p)", [Tail, ArgValue, Opt, Eos]),
    %% for positionals, strip the matched one now
    action(Tail, Current, ArgValue, Opt, Eos#eos{pos = tl(Eos#eos.pos)});

%% no more arguments for consumption, but last optional may still be action-ed
consume([], Current, Opt, Eos) ->
    ct:pal("EOL - looking into ~p (~200p)", [Opt, Eos]),
    action([], Current, undefined, Opt, Eos).

%% smart split: ignore options that can be parsed as negative numbers,
%%  unless there are options that look like negative numbers
split_to_option([], _, _Opts, Acc) ->
    {lists:reverse(Acc), []};
split_to_option(Rem, 0, _Opts, Acc) ->
    {lists:reverse(Acc), Rem};
split_to_option([[$- | _] = MaybeNumber | Tail] = All, Left, Opts, Acc) ->
    case is_digits(MaybeNumber) andalso no_digits(Opts) of
        true ->
            split_to_option(Tail, Left - 1, Opts, [MaybeNumber | Acc]);
        false ->
            {lists:reverse(Acc), All}
    end;
split_to_option([Head | Tail], Left, Opts, Acc) ->
    split_to_option(Tail, Left - 1, Opts, [Head | Acc]).

%%--------------------------------------------------------------------
%% Action handling

action(Tail, CmdMap, ArgValue, #{name := ArgName, action := set} = Opt, #eos{argmap = ArgMap} = Eos) ->
    Value = convert_type(maps:get(type, Opt, string), ArgValue, ArgName, Eos),
    parse_impl(Tail, CmdMap, Eos#eos{argmap = ArgMap#{ArgName => Value}});

action(Tail, CmdMap, undefined, #{name := ArgName, action := {store, Value}}, #eos{argmap = ArgMap} = Eos) ->
    parse_impl(Tail, CmdMap, Eos#eos{argmap = ArgMap#{ArgName => Value}});

action(Tail, CmdMap, ArgValue, #{name := ArgName, action := append} = Opt, #eos{argmap = ArgMap} = Eos) ->
    Value = convert_type(maps:get(type, Opt, string), ArgValue, ArgName, Eos),
    parse_impl(Tail, CmdMap, Eos#eos{argmap = ArgMap#{ArgName => maps:get(ArgName, ArgMap, []) ++ [Value]}});

action(Tail, CmdMap, undefined, #{name := ArgName, action := {append, Value}}, #eos{argmap = ArgMap} = Eos) ->
    parse_impl(Tail, CmdMap, Eos#eos{argmap = ArgMap#{ArgName => maps:get(ArgName, ArgMap, []) ++ [Value]}});

action(Tail, CmdMap, ArgValue, #{name := ArgName, action := extend} = Opt, #eos{argmap = ArgMap} = Eos) ->
    Value = convert_type(maps:get(type, Opt, string), ArgValue, ArgName, Eos),
    Extended = maps:get(ArgName, ArgMap, []) ++ Value,
    parse_impl(Tail, CmdMap, Eos#eos{argmap = ArgMap#{ArgName => Extended}});

action(Tail, CmdMap, undefined, #{name := ArgName, action := count}, #eos{argmap = ArgMap} = Eos) ->
    parse_impl(Tail, CmdMap, Eos#eos{argmap = ArgMap#{ArgName => maps:get(ArgName, ArgMap, 0) + 1}});

%% default: same as set
action(Tail, CmdMap, ArgValue, Opt, Eos) ->
    action(Tail, CmdMap, ArgValue, Opt#{action => set}, Eos).

%%--------------------------------------------------------------------
%% Type conversion

%% Handle "list" variant
convert_type({list, Type}, Arg, Opt, Eos) ->
    [convert_type(Type, Var, Opt, Eos) || Var <- Arg];

%% raw - no conversion applied (most likely default)
convert_type(raw, Arg, _Opt, _Eos) ->
    Arg;

%% Handle actual types
convert_type(string, Arg, _Opt, _Eos) ->
    Arg;
convert_type({string, Re}, Arg, Opt, Eos) ->
    case re:run(Arg, Re) of
        match -> Arg;
        {match, _X} -> Arg;
        _ -> error({invalid_argument, Eos#eos.path, Opt, Arg})
    end;
convert_type({string, Re, ReOpt}, Arg, Opt, Eos) ->
    case re:run(Arg, Re, ReOpt) of
        match -> Arg;
        {match, _} -> Arg;
        _ -> error({invalid_argument, Eos#eos.path, Opt, Arg})
    end;
convert_type(int, Arg, Opt, Eos) ->
    get_int(Arg, Opt, Eos);
convert_type({int, Opts}, Arg, Opt, Eos) ->
    minimax(get_int(Arg, Opt, Eos), Opts, Eos#eos.path, Opt);
convert_type(boolean, "true", _Opt, _Eos) ->
    true;
convert_type(boolean, "false", _Opt, _Eos) ->
    false;
convert_type(boolean, Other, Opt, #eos{path = Path}) ->
    error({invalid_argument, Path, Opt, Other});
convert_type(binary, Arg, _Opt, _Eos) ->
    list_to_binary(Arg);
convert_type({binary, Re}, Arg, Opt, Eos) ->
    case re:run(Arg, Re) of
        match -> list_to_binary(Arg);
        {match, _X} -> list_to_binary(Arg);
        _ -> error({invalid_argument, Eos#eos.path, Opt, Arg})
    end;
convert_type({binary, Re, ReOpt}, Arg, Opt, Eos) ->
    case re:run(Arg, Re, ReOpt) of
        match -> list_to_binary(Arg);
        {match, _} -> list_to_binary(Arg);
        _ -> error({invalid_argument, Eos#eos.path, Opt, Arg})
    end;
convert_type(float, Arg, Opt, Eos) ->
    get_float(Arg, Opt, Eos);
convert_type({float, Opts}, Arg, Opt, Eos) ->
    minimax(get_float(Arg, Opt, Eos), Opts, Eos#eos.path, Opt);
convert_type(atom, Arg, Opt, Eos) ->
    try list_to_existing_atom(Arg)
    catch error:badarg ->
        error({invalid_argument, Eos#eos.path, Opt, Arg})
    end;
convert_type({atom, unsafe}, Arg, _Opt, _Eos) ->
    list_to_atom(Arg);
convert_type({custom, Fun}, Arg, _Opt, _Eos) ->
    Fun(Arg);
convert_type(_Type, Arg, Opt, #eos{path = Path}) ->
    error({invalid_argument, Path, Opt, Arg}).

minimax(Var, [], _Path, _Opt) ->
    Var;
minimax(Var, [{min, Min} | _], Path, Opt) when Var < Min ->
    error({invalid_argument, Path, Opt, Var});
minimax(Var, [{max, Max} | _], Path, Opt) when Var > Max ->
    error({invalid_argument, Path, Opt, Var});
minimax(Var, [_ | Tail], Path, Opt) ->
    minimax(Var, Tail, Path, Opt).

%% returns int from string, or errors out
get_int(Arg, Opt, Eos) ->
    case string:to_integer(Arg) of
        {Int, []} ->
            Int;
        {error, _Reason} ->
            error({invalid_argument, Eos#eos.path, Opt, Arg})
    end.

%% returns float from string, that is floating-point, or integer
get_float(Arg, Opt, Eos) ->
    case string:to_float(Arg) of
        {Float, []} ->
            Float;
        {error, _Reason} ->
            %% possibly in disguise
            case string:to_integer(Arg) of
                {Int, []} ->
                    Int;
                {error, _IntReason} ->
                    error({invalid_argument, Eos#eos.path, Opt, Arg})
            end
    end.

%% Returns 'true' if String can be converted to a number
is_digits(String) ->
    case string:to_integer(String) of
        {_Int, []} ->
            true;
        {error, _} ->
            case string:to_float(String) of
                {_Float, []} ->
                    true;
                {error, _} ->
                    false
            end
    end.

%% returns 'true' if OptMap has no options that look like a negative number
no_digits(OptMap) ->
    lists:all(
        fun (#{short := Short}) when Short >= $0, Short =< $9 ->
                false;
            (#{long := Long}) ->
                not is_digits(Long);
            (_) ->
                true
        end,
        OptMap).

%% specially for 'maybe' (as '?') - produce a default argument, if it's there,
%%  and if it's not, produce what is guessed to be the default value of a type
default(#{default := Default}) ->
    Default;
default(#{type := boolean}) ->
    true;
default(#{type := int}) ->
    0;
default(#{type := float}) ->
    0.0;
default(#{type := string}) ->
    "";
default(#{type := atom}) ->
    undefined;
%% no type given, consider it 'undefined' atom
default(_) ->
    undefined.

%%--------------------------------------------------------------------
%% Validation, and preprocessing when needed

%% validates commands, throws invalid_command or invalid_option error
validate_command([{Name, Cmd} | _] = Path) ->
    is_atom(Name) orelse
        error({invalid_command, Path, commands, "command name must be an atom"}),
    is_map(Cmd) orelse
        error({invalid_command, Path, commands, "command description must be a map"}),
    is_list(maps:get(help, Cmd, [])) orelse
        error({invalid_command, Path, help, "help must be a string"}),
    is_map(maps:get(commands, Cmd, #{})) orelse
        error({invalid_command, Path, commands, "sub-commands must be a map"}),
    is_map_key(handler, Cmd) andalso (not is_function(maps:get(handler, Cmd))) andalso
        error({invalid_command, Path, handler, "handler must be a function accepting single map argument"}),
    Cmd1 =
        case maps:find(options, Cmd) of
            error ->
                Cmd;
            {ok, Opts} ->
                Cmd#{options => [validate_option(Path, Opt) || Opt <- Opts]}
        end,
    %% collect all short & long option identifiers - to figure out any conflicts
    lists:foldl(
        fun ({_, #{options := Opts}}, Acc) ->
            lists:foldl(
                fun (#{short := Short, name := OName}, {AllS, AllL}) ->
                        is_map_key(Short, AllS) andalso
                            error({invalid_option, Path, OName,
                                    "short conflicting with " ++ atom_to_list(maps:get(Short, AllS))}),
                        {AllS#{Short => OName}, AllL};
                    (#{long := Long, name := OName}, {AllS, AllL}) ->
                        is_map_key(Long, AllL) andalso
                            error({invalid_option, Path, OName,
                                    "long conflicting with " ++ atom_to_list(maps:get(Long, AllL))}),
                        {AllS, AllL#{Long => OName}};
                    (_, AccIn) ->
                        AccIn
                end, Acc, Opts);
            (_, Acc) ->
                Acc
        end, {#{}, #{}}, Path),
    %% verify all sub-commands
    case maps:find(commands, Cmd1) of
        error ->
            Cmd1;
        {ok, Sub} ->
            Cmd1#{commands => maps:map(fun (K, V) -> validate_command([{K, V} | Path]) end, Sub)}
    end.

%{invalid_option, cmd_path(), option(), Field :: atom(), Reason :: string()} |
validate_option(Path, #{name := Name} = Opt) when is_atom(Name) ->
    %% options cannot have unrecognised map items
    Unknown = maps:keys(maps:without([name, help, short, long, action, nargs, type, default, required], Opt)),
    Unknown =/= [] andalso error({invalid_option, Path, Opt, hd(Unknown), "unrecognised field"}),
    %% verify specific arguments
    is_list(maps:get(help, Opt, [])) orelse
        error({invalid_option, Path, Opt, help, "help must be a string"}),
    is_list(maps:get(long, Opt, [])) orelse
        error({invalid_option, Path, Opt, long, "long must be a string"}),
    is_boolean(maps:get(required, Opt, true)) orelse
        error({invalid_option, Path, Opt, required, "'required' must be boolean"}),
    is_integer(maps:get(short, Opt, $a)) orelse
        error({invalid_option, Path, Opt, short, "short must be character"}),
    Opt1 = maybe_validate(action, Opt, fun validate_action/3, Path),
    Opt2 = maybe_validate(type, Opt1, fun validate_type/3, Path),
    maybe_validate(nargs, Opt2, fun validate_args/3, Path);
validate_option(Path, Opt) ->
    error({invalid_option, Path, Opt, name, "option must be a map, with name := atom()"}).

maybe_validate(Key, Map, Fun, Path) when is_map_key(Key, Map) ->
    maps:put(Key, Fun(maps:get(Key, Map), Path, Map), Map);
maybe_validate(_Key, Map, _Fun, _Path) ->
    Map.

validate_action(store, _Path, _Opt) -> store;
validate_action({store, Term}, _Path, _Opt) -> {store, Term};
validate_action(append, _Path, _Opt) -> append;
validate_action({append, Term}, _Path, _Opt) -> {append, Term};
validate_action(count, _Path, _Opt) -> count;
validate_action(extend, _Path, _Opt) -> extend;
validate_action(_Action, Path, Opt) ->
    error({invalid_option, Path, Opt, action, "action is not valid"}).

validate_type(Simple, _Path, _Opt) when Simple =:= boolean; Simple =:= int; Simple =:= float;
    Simple =:= string; Simple =:= binary; Simple =:= atom; Simple =:= {atom, unsafe} ->
    Simple;
validate_type({custom, Fun}, _Path, _Opt) when is_function(Fun, 1) ->
    {custom, Fun};
validate_type({float, Opts}, Path, Opt) ->
    [error({invalid_option, Path, Opt, Opts, "invalid validator"})
        || {Kind, Val} <- Opts, (Kind =/= min andalso Kind =/= max) orelse (not is_float(Val))],
    {float, Opts};
validate_type({int, Opts}, Path, Opt) ->
    [error({invalid_option, Path, Opt, Opts, "invalid validator"})
        || {Kind, Val} <- Opts, (Kind =/= min andalso Kind =/= max) orelse (not is_integer(Val))],
    {float, Opts};
validate_type({string, Re} = Valid, _Path, _Opt) when is_list(Re) ->
    Valid;
validate_type({string, Re, L} = Valid, _Path, _Opt) when is_list(Re), is_list(L) ->
    Valid;
validate_type({binary, Re} = Valid, _Path, _Opt) when is_binary(Re) ->
    Valid;
validate_type({binary, Re, L} = Valid, _Path, _Opt) when is_binary(Re), is_list(L) ->
    Valid;
validate_type(_Type, Path, Opt) ->
    error({invalid_option, Path, Opt, type, "type is not supported"}).

validate_args(N, _Path, _Opt) when is_integer(N), N >= 1 -> N;
validate_args(Simple, _Path, _Opt) when Simple =:= all; Simple =:= list; Simple =:= maybe; Simple =:= nonempty_list ->
    Simple;
validate_args({maybe, Term}, _Path, _Opt) -> {maybe, Term};
validate_args(_Nargs, Path, Opt) ->
    error({invalid_option, Path, Opt, args, "'nargs' is not valid"}).
