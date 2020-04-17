%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Maxim Fedorov <maximfca@mail.com>
%%% @doc
%%% Command line utility behaviour. Usage example:
%%%
%%%  From an escript main/1 function (requires -mode(compile)):
%%%     cli:run(Args).
%%%
%%%  Or, to limit cli behaviur discovery,
%%%     cli:run(Args, #{modules => ?MODULE}).
%%%
%%% Warnings are printed to OTP logger, unless suppressed.
%%%
%%% cli framework attempts to create a handler for each
%%%     command exported, including intermediate (non-leaf)
%%%     commands, if it can find function exported with
%%%     suitable signature.
%%%
%%% @end
%%%

-module(cli).
-author("maximfca@gmail.com").

-export([
    run/1,
    run/2
]).

%%--------------------------------------------------------------------
%% Behaviour definition

%% @doc
%% Must return a command, that may contain sub-commands.
%% If there are no sub-commands, and there are no other
%%  modules loaded that implement cli behaviour,
%%  then cli/1 callback should also be defined, as it
%%  represents new entry point.
-callback cli() -> argparse:command().

%% @doc
%% Small utility that does not need sub-command should
%%  export this callback that will be called by run/1.
-callback cli(argparse:arg_map()) -> term().

%% @doc
%% Needs to be exported by small command line scripts that
%%  do not have any sub-commands.
-optional_callbacks([cli/1]).

%%--------------------------------------------------------------------
%% API

-compile(warn_missing_spec).

%% @doc
%% Finds all modules loaded, and implementing cli behaviour,
%%  then matches a command and runs handler defined for
%%  a command.
-spec run([string()]) -> term().
run(Args) ->
    run(Args, #{}).

%% @doc
%% Options map.
%% Allows to choose which modules to consider, and error handling mode.
%% 'modules' can be:
%%     'all_loaded' - code:all_loaded(), search for 'cli' behaviour,
%%     module()       for a single module (may not have 'cli' behaviour),
%%     [module()]     for a list of modules (may not have 'cli' behaviour)
%% 'warn' set to 'suppress' suppresses warnings logged
%% 'help' set to false suppresses printing 'usage' when parser produces
%%      an error, and disabled default --help/-h behaviour
%% 'handler' controls which handler form is going to be used to generate
%%      default handler, and value to use for missing arguments
-type run_options() :: #{
    modules => all_loaded | module() | [module()],
    warn => suppress | warn,
    help => boolean(),
    default => term(),
    prefixes => [integer()],%% prefixes passed to argparse
    progname => string()    %% specifies executable name instead of 'erl'
}.

%% @doc
%% Fine-grained control version of run/1, supporting options.
%% Returns either a term, or term with list of exceptions turned into
%%  tuples of {Class, Reason, [Stack]}
-spec run([string()], run_options()) -> term().
run(Args, Options) ->
    ParserOptions = copy_options([prefixes, progname], Options, #{}),
    run_impl(Args, ParserOptions,
        modules(maps:get(modules, Options, all_loaded)), Options).

%%--------------------------------------------------------------------
%% Internal implementation

%% @doc conditionally copy items from one map to another
%% next 5 lines of code make Dialyzer happy. Otherwise
%%  it rightfully says "your parser options also contain
%%  run options"
copy_options([], _Options, Acc) ->
    Acc;
copy_options([Head|Tail], Options, Acc) when is_map_key(Head, Options) ->
    copy_options(Tail, Options, Acc#{Head => maps:get(Head, Options)});
copy_options([_Head|Tail], Options, Acc) ->
    copy_options(Tail, Options, Acc).

-include_lib("kernel/include/logger.hrl").

%% @private
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

%% @private
%% Dispatches Args over Modules, with specified ErrMode
run_impl(Args, ArgOpts, Modules, Options) ->
    Warn = maps:get(warn, Options, warn),
    HelpEnabled = maps:get(help, Options, false),
    ModCount = length(Modules),
    CmdMap = lists:foldl(
        fun (Mod, Cmds) ->
            ModCmd =
                try Mod:cli()
                catch
                    Class:Reason:Stack when Warn =:= warn ->
                        ?LOG_WARNING("Error calling ~s:cli(): ~s:~p~n~p",
                            [Mod, Class, Reason, Stack]), #{};
                        _:_ when Warn =:= suppress ->
                            #{}
                end,
            %% handlers: use first non-empty handler
            Cmds1 =
                if (not is_map_key(handler, Cmds)) andalso is_map_key(handler, ModCmd) ->
                    Cmds#{handler => maps:get(handler, ModCmd)};
                    true -> Cmds
                end,
            %% help: concatenate help lines
            Cmds2 =
                if is_map_key(help, ModCmd) ->
                    Cmds1#{help => maps:get(handler, ModCmd) ++ maps:get(help, Cmds1, "")};
                    true -> Cmds1
                end,
            %% merge arguments, and warn if warnings are not suppressed, and there
            %%  is more than a single module
            Cmds3 = merge_arguments(maps:get(arguments, ModCmd, []),
                (ModCount > 1 andalso Warn =:= warn), Cmds2),
            %% merge commands
            merge_commands(maps:get(commands, ModCmd, #{}), Mod, Options, Cmds3)
        end, #{}, Modules),
    %% attempt to dispatch the command
    try argparse:parse(Args, CmdMap, ArgOpts) of
        {ArgMap, {Path, #{handler := {Mod, ModFun, Default}}}} ->
            ArgList = arg_map_to_arg_list(CmdMap, Path, ArgMap, Default),
            %% if argument count may not match, better error can be produced
            erlang:apply(Mod, ModFun, ArgList);
        {ArgMap, {_Path, #{handler := {Mod, ModFun}}}} when is_atom(Mod), is_atom(ModFun) ->
            Mod:ModFun(ArgMap);
        {ArgMap, {Path, #{handler := {Fun, Default}}}} when is_function(Fun) ->
            ArgList = arg_map_to_arg_list(CmdMap, Path, ArgMap, Default),
            %% if argument count may not match, better error can be produced
            erlang:apply(Fun, ArgList);
        {ArgMap, {_Path, #{handler := Handler}}} when is_function(Handler, 1) ->
            Handler(ArgMap);
        ArgMap ->
            %% simple CLI with no sub-commands?
            case maps:find(default, Options) of
                error ->
                    %% find any module of Modules, exporting cli/1,
                    %%  and call it
                    exec_cli(Modules, CmdMap, [ArgMap], ArgOpts);
                {ok, Default} ->
                    ArgList = arg_map_to_arg_list(CmdMap, [], ArgMap, Default),
                    exec_cli(Modules, CmdMap, ArgList, ArgOpts)
            end
    catch
        error:{argparse, Reason} when HelpEnabled ->
            io:format("error: ~s", [argparse:format_error(Reason)]);
        error:{argparse, Reason} ->
            %% see if it was cry for help that triggered error message
            Prefixes = maps:get(prefixes, ArgOpts, "-"),
            case help_requested(Reason, Prefixes) of
                false ->
                    Fmt = argparse:format_error(Reason, CmdMap, ArgOpts),
                    io:format("error: ~s", [Fmt]);
                CmdPath ->
                    Fmt = argparse:help(CmdMap, ArgOpts#{command => tl(CmdPath)}),
                    io:format("~s", [Fmt])
            end
    end.

%% @private
%% finds first module that exports ctl/1 and execute it
exec_cli([], CmdMap, _ArgMap, ArgOpts) ->
    %% command not found, let's print usage
    io:format(argparse:help(CmdMap, ArgOpts));
exec_cli([Mod|Tail], CmdMap, Args, ArgOpts) ->
    case erlang:function_exported(Mod, cli, length(Args)) of
        true ->
            erlang:apply(Mod, cli, Args);
        false ->
            exec_cli(Tail, CmdMap, Args, ArgOpts)
    end.

%% @private
%% argparse does not allow clashing options, so if cli is ever to support
%%  that, logic to un-clash should be here
merge_arguments([], _Warn, Existing) ->
    Existing;
merge_arguments(Args, Warn, Existing) ->
    Warn andalso
        ?LOG_WARNING("cli: multiple modules may export global attributes: ~p", [Args]),
    ExistingArgs = maps:get(arguments, Existing, []),
    Existing#{arguments => ExistingArgs ++ Args}.

%% @private
%% argparse accepts a map of commands, which means, commands names
%%  can never clash. Yet for cli it is possible when multiple modules
%%  export command with the same name. For this case, skip duplicate
%%  command names, emitting a warning.
merge_commands([], _, _, Existing) ->
    Existing;
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

%% @private
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

%% @private makes handler in required format
make_handler(CmdName, Mod, error) ->
    {Mod, list_to_existing_atom(CmdName)};
make_handler(CmdName, Mod, {ok, Default}) ->
    {Mod, list_to_existing_atom(CmdName), Default}.

%% @private
%% Finds out whether it was --help/-h requested, and exception was thrown due to that
help_requested({unknown_argument, CmdPath, [Prefix, $h]}, Prefixes) ->
    is_prefix(Prefix, Prefixes, CmdPath);
help_requested({unknown_argument, CmdPath, [Prefix, Prefix, $h, $e, $l, $p]}, Prefixes) ->
    is_prefix(Prefix, Prefixes, CmdPath);
help_requested(_, _) ->
    false.

%% @private returns CmdPath when Prefix is one of supplied Prefixes
is_prefix(Prefix, Prefixes, CmdPath) ->
    case lists:member(Prefix, Prefixes) of
        true ->
            CmdPath;
        false ->
            false
    end.

%% @private
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
