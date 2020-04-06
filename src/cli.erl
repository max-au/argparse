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

-callback cli() -> argparse:command().

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
%%    'all_loaded' - code:all_loaded(), search for 'cli' behaviour,
%%    module()       for a single module (may not have 'cli' behaviour),
%%    [module()]     for a list of modules (may not have 'cli' behaviour)
%% 'warn' set to 'suppress' suppresses warnings logged
-type run_options() :: #{
    modules => all_loaded | module() | [module()],
    warn => suppress | warn
}.

%% @doc
%% Fine-grained control version of run/1, supporting options.
%% Returns either a term, or term with list of exceptions turned into
%%  tuples of {Class, Reason, [Stack]}
-spec run([string()], run_options()) -> term().
run(Args, Options) ->
    run_impl(Args,
        modules(maps:get(modules, Options, all_loaded)),
        maps:get(warn, Options, warn)).

%%--------------------------------------------------------------------
%% Internal implementation

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
run_impl(Args, Modules, Warn) ->
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
    try argparse:parse(Args, CmdMap) of
        {ArgMap, {_Command, #{handler := Handler}}} ->
            Handler(ArgMap);
        ArgMap ->
            %% simple CLI with no sub-commands?
            %% find any module of Modules, exporting cli/1,
            %%  and call it
            find_ctl(Modules, CmdMap, ArgMap)
    catch error:{argparse, Reason} ->
        Fmt = argparse:format_error(Reason, CmdMap),
        io:format("error: ~s", [Fmt])
    end.

%% @private
%% finds first module that exports ctl/1 and execute it
find_ctl([], CmdMap, _ArgMap) ->
    %% command not found, let's print usage
    io:format(argparse:help(CmdMap, #{}));
find_ctl([Mod|Tail], CmdMap, ArgMap) ->
    case erlang:function_exported(Mod, cli, 1) of
        true ->
            Mod:cli(ArgMap);
        false ->
            find_ctl(Tail, CmdMap, ArgMap)
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
                        [Name, 8, Another]),
                    Acc;
                {ok, _Another} when Warn =:= suppress ->
                    Acc
            end
        end, maps:get(commands, Existing, #{}), Cmds
    ),
    Existing#{commands => MergedCmds}.


