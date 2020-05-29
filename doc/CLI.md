# cli mini-framework
cli is designed to simplify command-line interface integration. From a tiny escript,
to a system exporting several hundred commands.

## Basic [example](examples/escript/simple)

Implementing a utility with a single command to run requires:
1. Compiled code (as evaluated code does not work with callbacks)
2. 'cli' behaviour declared
3. cli/0 callback returning arguments map and a handler


    #!/usr/bin/env escript

    -export([main/1, cli/0, cli/1]).
    -behaviour(cli).
    -mode(compile). %% evaluated module cannot contain callbacks

    main(Args) ->
        cli:run(Args, #{progname => "simple").

    cli() ->
        #{arguments => [
            #{name => force, short => $f, type => boolean, default => false},
            #{name => recursive, short => $r, type => boolean, default => false},
            #{name => dir}
        ]}.

    cli(#{force := Force, recursive := Recursive, dir := Dir}) ->
        io:format("Removing ~s (force ~s, recursive: ~s)~n",
            [Dir, Force, Recursive]).

## rebar3 escript [example](examples/conf_reader)
Creating a new application exposing CLI is as simple as:
1. Running `rebar3 new escript conf_reader`
2. Adding `argparse` to `deps` and `escript_incl_apps` in rebar.config
3. Add a function (`cli/0`) declaring CLI arguments
4. Use the specification: `argparse:parse(Args, cli())`
5. Run `rebar3 escriptize` to build the application.

## Command-line interface discovery

By default, ```cli:run/1``` scans all loaded modules to find those implementing
**cli** behaviour.
Use ```run/2``` with **modules** option to specify a single module, or a
list of modules to search (default is **all_loaded**):

    cli:run(["arg"], #{modules => ?MODULE}).

When there are multiple modules in the list, cli merges all arguments and
commands exported by these modules. In case of a conflict (e.g. clashing
short or long option name, or top-level command name), first match is
accepted, and all others are discarded (with warning emitted to OTP logger,
can be switch off with **warn** flag set to false).

Be careful with top-level arguments exported: they are added to
*all* commands produced by *all* modules - this side effect is usually
undesired. There is a warning emitted if more than one module implements
cli behaviour, and global arguments are exported.


## Handler specification

Handler is a callback invoked by cli:run when command line parser completes successfully.

Handler may accept a single argument (map of argument names to their values, as returned
by argparse):

    sum(#{numbers := Numbers}) ->
        lists:sum(Numbers).

In this case, command spec should define handler either as fun, or as a tuple
```{module(), atom()}```, this requires function to be exported:

    %% map form:
    cli() -> #{commands => {"sum" => #{handler => fun sum/1}}}.

    %% exported function, map form:
    cli() -> #{commands => {"sum" => #{handler => {?MODULE, sum}}}}.

Handler may accept positional arguments:

    start(Server, Mode) ->
        supervisor:start_child(my_server_sup,
            #{id => Server, start => {my_server, start_link, [Mode]}}).

In this case handler specification must define default value for missing arguments:

    handler => {fun start/2, undefined}
    %% ... or, for exported function:
    handler => {?MODULE, start, undefined}

## Default handler

When handler is not specified, cli generates default one, based on the module
implementing cli behaviour, and **default** field passed to ```run/2```. When
the field it set, positional form for handler is generated, with default set to
the term supplied.

cli does not check whether handler function exists or exported.

## Help/usage information

By default, when parser encounters an error, cli generates both error message and help/usage
information. It is possible to suppress usage messages by providing **help** option
set to *false*. This also disables usage output for *--help* or *-h* request.


As most escripts are interpreted via *erl*, it is recommended to define **progname**
to provide correct help/usage line:

    cli:run(["arg"], #{progname => "mycli"}).

## Reference

cli is able to pass **prefixes** option to argparse (this also changes *-h* and *--help*
prefix).