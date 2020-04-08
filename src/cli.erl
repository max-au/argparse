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
-type run_options() :: #{
    modules => all_loaded | module() | [module()],
    warn => suppress | warn,
    help => boolean(),
    prefixes => [integer()],%% prefixes
    progname => string()    %% specifies executable name instead of 'erl'
}.

%% @doc
%% Fine-grained control version of run/1, supporting options.
%% Returns either a term, or term with list of exceptions turned into
%%  tuples of {Class, Reason, [Stack]}
-spec run([string()], run_options()) -> term().
run(Args, Options) ->
    ParserOptions = copy_options([prefixes, progname], Options, #{}),
    run_impl(Args,
        modules(maps:get(modules, Options, all_loaded)),
        maps:get(warn, Options, warn),
        ParserOptions, maps:get(help, Options, false)).

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
run_impl(Args, Modules, Warn, ArgOpts, HelpEnabled) ->
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
            Cmds1 = merge_arguments(maps:get(arguments, ModCmd, []), Cmds),
            merge_commands(maps:get(commands, ModCmd, #{}), Mod, Warn, Cmds1)
        end, #{}, Modules),
    %% attempt to dispatch the command
    try argparse:parse(Args, CmdMap, ArgOpts) of
        {ArgMap, {_Command, #{handler := Handler}}} ->
            Handler(ArgMap);
        ArgMap ->
            %% simple CLI with no sub-commands?
            %% find any module of Modules, exporting cli/1,
            %%  and call it
            find_ctl(Modules, CmdMap, ArgMap, ArgOpts)
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
find_ctl([], CmdMap, _ArgMap, ArgOpts) ->
    %% command not found, let's print usage
    io:format(argparse:help(CmdMap, ArgOpts));
find_ctl([Mod|Tail], CmdMap, ArgMap, ArgOpts) ->
    case erlang:function_exported(Mod, cli, 1) of
        true ->
            Mod:cli(ArgMap);
        false ->
            find_ctl(Tail, CmdMap, ArgMap, ArgOpts)
    end.

%% @private
%% argparse does not allow clashing options, so if cli is ever to support
%%  that, logic to un-clash should be here
merge_arguments([], Existing) ->
    Existing;
merge_arguments(Args, Existing) ->
    ExistingArgs = maps:get(arguments, Existing, []),
    Existing#{arguments => ExistingArgs ++ Args}.

%% @private
%% argparse accepts a map of commands, which means, commands names
%%  can never clash. Yet for cli it is possible - resolve conflict here.
merge_commands([], _, _, Existing) ->
    Existing;
merge_commands(Cmds, Mod, Warn, Existing) ->
    MergedCmds = maps:fold(
        fun (Name, Cmd, Acc) ->
            case maps:find(Name, Acc) of
                error when is_map_key(handler, Cmd) ->
                    Acc#{Name => Cmd#{mod => Mod}};
                error ->
                    FunAtom = list_to_existing_atom(Name),
                    Acc#{Name => Cmd#{handler => fun(AM) -> Mod:FunAtom(AM) end}};
                {ok, Another} when Warn =:= warn ->
                    ?LOG_WARNING("cli: duplicate definition for ~s found, skipping ~P",
                        [Name, 8, Another]), Acc;
                {ok, _Another} when Warn =:= suppress ->
                    Acc
            end
        end, maps:get(commands, Existing, #{}), Cmds
    ),
    Existing#{commands => MergedCmds}.

%% @private
%% Finds out whether it was --help/-h requested, and exception was thrown due to that
help_requested({unknown_argument, CmdPath, [Prefix, $h]}, Prefixes) ->
    is_prefix(Prefix, Prefixes, CmdPath);
help_requested({unknown_argument, CmdPath, [Prefix, Prefix, $h, $e, $l, $p]}, Prefixes) ->
    is_prefix(Prefix, Prefixes, CmdPath);
help_requested(_, _) ->
    false.

is_prefix(Prefix, Prefixes, CmdPath) ->
    case lists:member(Prefix, Prefixes) of
        true ->
            CmdPath;
        false ->
            false
    end.
