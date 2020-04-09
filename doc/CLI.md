# cli Framework
cli framework is designed to simplify command-line interface integration. From a tiny escript,
to a system exporting several hundred commands.

## Basic example

## Advanced example

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