%%%-------------------------------------------------------------------
%%% @author Maxim Fedorov, <maximfca@gmail.com>
%%% @doc
%%% Command line utility behaviour. Usage example:
%%%
%%%  From an escript main/1 function (requires `-mode(compile)'):
%%%  ```
%%%     cli:run(Args).
%%%  '''
%%%
%%%  Or, to limit cli behaviour discovery,
%%%  ```
%%%     cli:run(Args, #{modules => ?MODULE, progname => ?MODULE}).
%%%  '''
%%% Other options available for run/2:
%%% <ul><li>`modules':<ul>
%%%     <li>`all_loaded' - search all loaded modules (`code:all_loaded()') for `cli' behaviour</li>
%%%     <li>`module()'   - use this module (must export `cli/0')</li>
%%%     <li>[module()]   - list of modules (must export `cli/0')</li></ul></li>
%%% <li>`warn': set to `suppress' suppresses warnings logged</li>
%%% <li>`error': defines what action is taken upon parser error. Use `ok' to completely ignore the
%%%      error (historical behaviour, useful for testing), `error' to raise an exception,
%%%      `halt' to halt the emulator with exit code 1 (default behaviour), and `{halt, non_neg_integer()}'
%%%      for a custom exit code halting the emulator</li>
%%% <li>`help': set to false suppresses printing `usage' when parser produces
%%%      an error, and disables default --help/-h behaviour</li>
%%% <li>`prefixes': prefixes passed to argparse</li>
%%% <li>`progname': specifies executable name instead of 'erl'</li>
%%% </ul>
%%%
%%% Warnings are printed to OTP logger, unless suppressed.
%%%
%%% cli framework attempts to create a handler for each
%%%     command exported, including intermediate (non-leaf)
%%%     commands, if it can find function exported with
%%%     suitable signature.
%%%
%%% cli examples are <a href="https://github.com/max-au/argparse/tree/master/doc/examples">available on GitHub</a>
%%%
%%% @end

-module(cli).
-author("maximfca@gmail.com").

-export([
    run/1,
    run/2
]).

%%--------------------------------------------------------------------
%% Behaviour definition

%% Callback returning CLI mappings.
%% Must return a command, that may contain sub-commands.
%% Also returns arguments, and handler.
-callback cli() -> args:command().

%%--------------------------------------------------------------------
%% API

-compile(warn_missing_spec).

-spec run(Args :: [string()]) -> term().
%% @equiv run(Args, #{})
run(Args) ->
    run(Args, #{}).

%% Options map.
%% Allows to choose which modules to consider, and error handling mode.
%% `modules' can be:
%%     `all_loaded' - code:all_loaded(), search for `cli' behaviour,
%%     module()       for a single module (may not have `cli' behaviour),
%%     [module()]     for a list of modules (may not have `cli' behaviour)
%% `warn' set to `suppress' suppresses warnings logged
%% `help' set to false suppresses printing `usage' when parser produces
%%      an error, and disables default --help/-h behaviour
-type run_options() :: #{
    modules => all_loaded | module() | [module()],
    warn => suppress | warn,
    help => boolean(),
    error => ok | error | halt | {halt, non_neg_integer()},
    prefixes => [integer()],%% prefixes passed to argparse
    %% default value for all missing not required arguments
    default => term(),
    progname => string() | atom()   %% specifies executable name instead of 'erl'
}.

%% @doc CLI entry point, parses arguments and executes selected function.
%% Finds all modules loaded, and implementing cli behaviour,
%%  then matches a command and runs handler defined for
%%  a command.
%% @param Args arguments used to run CLI, e.g. init:get_plain_arguments().
%% @returns callback result, or 'ok' when help/error message printed, and
%%  `error' parameter is set to `ok' (meaning, ignore errors, always return ok)
-spec run([string()], run_options()) -> term().
run(Args, Options) ->
    Modules = modules(maps:get(modules, Options, all_loaded)),
    CmdMap = discover_commands(Modules, Options),
    dispatch(Args, CmdMap, Modules, Options).

%%--------------------------------------------------------------------
%% Internal implementation

-include_lib("kernel/include/logger.hrl").

%% Returns a list of all modules providing 'cli' behaviour, or
%%  a list of modules.
modules(all_loaded) ->
    [
        Module || {Module, _} <- code:all_loaded(),
        lists:member(?MODULE, behaviours(Module))
    ];
modules(Mod) when is_atom(Mod) ->
    [Mod];
modules(Mods) when is_list(Mods) ->
    Mods.

behaviours(Module) ->
    Attrs = proplists:get_value(attributes, Module:module_info(), []),
    lists:flatten(proplists:get_all_values(behavior, Attrs) ++
        proplists:get_all_values(behaviour, Attrs)).

%%
discover_commands(Modules, Options) ->
    Warn = maps:get(warn, Options, warn),
    ModCount = length(Modules),
    lists:foldl(
        fun (Mod, Cmds) ->
            ModCmd =
                try {_, MCmd} = args:validate(Mod:cli(), Options), MCmd
                catch
                    Class:Reason:Stack when Warn =:= warn ->
                        ?LOG_WARNING("Error calling ~s:cli(): ~s:~p~n~p",
                            [Mod, Class, Reason, Stack]), #{};
                        _:_ when Warn =:= suppress ->
                            #{}
                end,
            %% handlers: use first non-empty handler
            Cmds1 = case maps:find(handler, ModCmd) of
                {ok, Handler} when is_map_key(handler, Cmds) ->
                    %% merge handler - and warn when not suppressed
                    Warn =:= warn andalso
                        ?LOG_WARNING("Multiple handlers defined for top-level command, ~p chosen, ~p ignored",
                            [maps:get(handler, Cmds), Handler]),
                    Cmds;
                {ok, Handler} ->
                    Cmds#{handler => Handler};
                error ->
                    Cmds
            end,
            %% help: concatenate help lines
            Cmds2 =
                if is_map_key(help, ModCmd) ->
                    Cmds1#{help => maps:get(help, ModCmd) ++ maps:get(help, Cmds1, "")};
                    true -> Cmds1
                end,
            %% merge arguments, and warn if warnings are not suppressed, and there
            %%  is more than a single module
            Cmds3 = merge_arguments(maps:get(arguments, ModCmd, []),
                (ModCount > 1 andalso Warn =:= warn), Cmds2),
            %% merge commands
            merge_commands(maps:get(commands, ModCmd, #{}), Mod, Options, Cmds3)
        end, #{}, Modules).

%% Dispatches Args over Modules, with specified ErrMode
dispatch(Args, CmdMap, Modules, Options) ->
    HelpEnabled = maps:get(help, Options, true),
    %% attempt to dispatch the command
    try args:parse(Args, CmdMap, Options) of
        {ArgMap, PathTo} ->
            run_handler(CmdMap, ArgMap, PathTo, undefined);
        ArgMap ->
            %{ maps:find(default, Options), Modules, Options}
            run_handler(CmdMap, ArgMap, {[], CmdMap}, {Modules, Options})
    catch
        error:{args, Reason} when HelpEnabled =:= false ->
            io:format("error: ~s", [args:format_error(Reason)]),
            dispatch_error(Options, Reason);
        error:{args, Reason} ->
            %% see if it was cry for help that triggered error message
            Prefixes = maps:get(prefixes, Options, "-"),
            case help_requested(Reason, Prefixes) of
                false ->
                    Fmt = args:format_error(Reason, CmdMap, Options),
                    io:format("error: ~s", [Fmt]);
                CmdPath ->
                    Fmt = args:help(CmdMap, Options#{command => tl(CmdPath)}),
                    io:format("~s", [Fmt])
            end,
            dispatch_error(Options, Reason)
    end.

dispatch_error(#{error := ok}, _Reason) ->
    ok;
dispatch_error(#{error := error}, Reason) ->
    error(Reason);
dispatch_error(#{error := halt}, _Reason) ->
    erlang:halt(1);
dispatch_error(#{error := {halt, Exit}}, _Reason) ->
    erlang:halt(Exit);
%% default is halt(1)
dispatch_error(_Options, _Reason) ->
    erlang:halt(1).

%% Executes handler
run_handler(CmdMap, ArgMap, {Path, #{handler := {Mod, ModFun, Default}}}, _MO) ->
    ArgList = arg_map_to_arg_list(CmdMap, Path, ArgMap, Default),
    %% if argument count may not match, better error can be produced
    erlang:apply(Mod, ModFun, ArgList);
run_handler(_CmdMap, ArgMap, {_Path, #{handler := {Mod, ModFun}}}, _MO) when is_atom(Mod), is_atom(ModFun) ->
    Mod:ModFun(ArgMap);
run_handler(CmdMap, ArgMap, {Path, #{handler := {Fun, Default}}}, _MO) when is_function(Fun) ->
    ArgList = arg_map_to_arg_list(CmdMap, Path, ArgMap, Default),
    %% if argument count may not match, better error can be produced
    erlang:apply(Fun, ArgList);
run_handler(_CmdMap, ArgMap, {_Path, #{handler := Handler}}, _MO) when is_function(Handler, 1) ->
    Handler(ArgMap);
run_handler(CmdMap, ArgMap, {[], _}, {Modules, Options}) ->
    % {undefined, {ok, Default}, Modules, Options}
    exec_cli(Modules, CmdMap, [ArgMap], Options).

%% finds first module that exports ctl/1 and execute it
exec_cli([], CmdMap, _ArgMap, ArgOpts) ->
    %% command not found, let's print usage
    io:format(args:help(CmdMap, ArgOpts));
exec_cli([Mod|Tail], CmdMap, Args, ArgOpts) ->
    case erlang:function_exported(Mod, cli, length(Args)) of
        true ->
            erlang:apply(Mod, cli, Args);
        false ->
            exec_cli(Tail, CmdMap, Args, ArgOpts)
    end.

%% argparse does not allow clashing options, so if cli is ever to support
%%  that, logic to un-clash should be here
merge_arguments([], _Warn, Existing) ->
    Existing;
merge_arguments(Args, Warn, Existing) ->
    Warn andalso
        ?LOG_WARNING("cli: multiple modules may export global attributes: ~p", [Args]),
    ExistingArgs = maps:get(arguments, Existing, []),
    Existing#{arguments => ExistingArgs ++ Args}.

%% argparse accepts a map of commands, which means, commands names
%%  can never clash. Yet for cli it is possible when multiple modules
%%  export command with the same name. For this case, skip duplicate
%%  command names, emitting a warning.
merge_commands(Cmds, Mod, Options, Existing) ->
    Warn = maps:get(warn, Options, warn),
    MergedCmds = maps:fold(
        fun (Name, Cmd, Acc) ->
            case maps:find(Name, Acc) of
                error ->
                    %% merge command with name Name into Acc-umulator
                    Acc#{Name => create_handlers(Mod, Name, Cmd, maps:find(default, Options))};
                {ok, Another} when Warn =:= warn ->
                    %% do not merge this command, another module already exports it
                    ?LOG_WARNING("cli: duplicate definition for ~s found, skipping ~P",
                        [Name, 8, Another]), Acc;
                {ok, _Another} when Warn =:= suppress ->
                    %% don't merge duplicate, and don't complain about it
                    Acc
            end
        end, maps:get(commands, Existing, #{}), Cmds
    ),
    Existing#{commands => MergedCmds}.

%% Descends into sub-commands creating handlers where applicable
create_handlers(Mod, CmdName, Cmd0, DefaultTerm) ->
    Handler =
        case maps:find(handler, Cmd0) of
            error ->
                make_handler(CmdName, Mod, DefaultTerm);
            {ok, optional} ->
                make_handler(CmdName, Mod, DefaultTerm);
            {ok, Existing} ->
                Existing
        end,
    %%
    Cmd = Cmd0#{handler => Handler},
    case maps:find(commands, Cmd) of
        error ->
            Cmd;
        {ok, Sub} ->
            NewCmds = maps:map(fun (CN, CV) -> create_handlers(Mod, CN, CV, DefaultTerm) end, Sub),
            Cmd#{commands => NewCmds}
    end.

%% makes handler in required format
make_handler(CmdName, Mod, error) ->
    try
        {Mod, list_to_existing_atom(CmdName)}
    catch
        error:badarg ->
            error({invalid_command, [CmdName], handler, "handler for command does not exist"})
    end;
make_handler(CmdName, Mod, {ok, Default}) ->
    {Mod, list_to_existing_atom(CmdName), Default}.

%% Finds out whether it was --help/-h requested, and exception was thrown due to that
help_requested({unknown_argument, CmdPath, [Prefix, $h]}, Prefixes) ->
    is_prefix(Prefix, Prefixes, CmdPath);
help_requested({unknown_argument, CmdPath, [Prefix, Prefix, $h, $e, $l, $p]}, Prefixes) ->
    is_prefix(Prefix, Prefixes, CmdPath);
help_requested(_, _) ->
    false.

%% returns CmdPath when Prefix is one of supplied Prefixes
is_prefix(Prefix, Prefixes, CmdPath) ->
    case lists:member(Prefix, Prefixes) of
        true ->
            CmdPath;
        false ->
            false
    end.

%% Given command map, path to reach a specific command, and a parsed argument
%%  map, returns a list of arguments (effectively used to transform map-based
%%  callback handler into positional).
arg_map_to_arg_list(Command, Path, ArgMap, Default) ->
    AllArgs = collect_arguments(Command, Path, []),
    [maps:get(Arg, ArgMap, Default) || #{name := Arg} <- AllArgs].

%% recursively descend into Path, ignoring arguments with duplicate names
collect_arguments(Command, [], Acc) ->
    Acc ++ maps:get(arguments, Command, []);
collect_arguments(Command, [H|Tail], Acc) ->
    Args = maps:get(arguments, Command, []),
    Next = maps:get(H, maps:get(commands, Command, H)),
    collect_arguments(Next, Tail, Acc ++ Args).
