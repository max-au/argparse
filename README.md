# argparse: command line parser for Erlang

A simple framework to create complex CLI. Inspired by Python argparse.

Follows conventions of  Unix Utility Argument Syntax.

    argparse [-abcDxyz][-p arg][operand]

## Simple CLI example

CLI framework is naturally suitable for building small escript-based applications:

    #!/usr/bin/env escript
    
    -export([main/1, cli/0, cli/1]).
    -behaviour(cli).
    -mode(compile). %% evaluated module cannot contain callbacks
    
    main(Args) ->
        cli:run(Args).
    
    cli() ->
        #{arguments => [
            #{name => force, short => $f, type => boolean, default => false},
            #{name => recursive, short => $r, type => boolean, default => false},
            #{name => dir}
        ]}.
    
    cli(#{force := Force, recursive := Recursive, dir := Dir}) ->
        io:format("Removing ~s (force ~s, recursive: ~s)~n",
            [Dir, Force, Recursive]).

## CLI with multiple commands

Argparse supports complex CLI with sub-commands. Calculator example:

    #!/usr/bin/env escript
    
    -export([main/1, cli/0, sum/1, cos/1]).
    -behaviour(cli).
    -mode(compile).
    
    main(Args) ->
        cli:run(Args).
    
    cli() ->
        #{
            commands => #{
                "sum" => #{
                    arguments => [
                        #{name => num, nargs => nonempty_list, type => int}
                    ]
                },
                "cos" => #{
                    arguments => [
                        #{name => in, type => float, help => "Input"}
                    ]
                }
            }
        }.
    
    sum(#{num := Nums}) ->
        io:format("~b~n", [lists:sum(Nums)]).
    
    cos(#{in := In}) ->
        io:format("~g~n", [math:cos(In)]).

Calculator provides "sum" command that prints a sum of integer numbers:

    ./calc sum 1 2 3
    6

Or a cosinus:

    /calc cos 1.4
    0.169967

## Argument parser

It is possible to use argument parser alone, without CLI framework:

    #!/usr/bin/env escript
    
    main(Args) ->
        #{force := Force, recursive := Recursive, dir := Dir} =
            argparse:parse(Args, cli()),
        io:format("Removing ~s (force: ~s, recursive: ~s)~n",
            [Dir, Force, Recursive]).
        
    cli() ->
        #{arguments => [
            #{name => force, short => $f, type => boolean, default => false},
            #{name => recursive, short => $r, type => boolean, default => false},
            #{name => dir}
        ]}.


## Parser Reference
Parser operates with *arguments* and *commands*, organised in a hierarchy. It is possible
to define multiple commands, or none. Parsing always starts with root *command*,
named after ```init:get_argument(progname)```.

    1> parse("", #{}).
    #{}

#### Options

It's possible to override program name using **progname** option:

    2> io:format(argparse:help(#{}, #{progname => "readme"})).
    usage: readme

To override default optional argument prefix (**-**), use **prefixes** option:

    3> argparse:parse(["+sbwt"], #{arguments => [#{name => mode, short => $s}]}, #{prefixes => "+"}).
    #{mode => "bwt"}

#### Validation, help & usage information

Function ```validate/1``` may be used to validate command with all sub-commands
and options without actual parsing done.

    4> argparse:validate(#{arguments => [#{short => $4}]}).
    ** exception error: {argparse,{invalid_option,["erl"],
        [],name,"argument must be a map, and specify 'name'"}}

Human-readable errors and usage information is accessible via ```help/2``` and ```format_error/1,2```.

#### Return value

If root level command does not contain any sub-commands, parser returns plain map of
argument names to their values:

    3> argparse:parse(["value"], #{arguments => [#{name => arg}]}).
    #{arg => "value"}

This map contains all arguments matching command line passed, initialised with
corresponding values. If argument is omitted, but default value is specified for it,
it is added to the map. When no default value specified, and argument is not
present, corresponding key is not present in the map.

Missing required (field **required** is set to true for optional arguments,
or missing for positional) arguments raises an error.

When there are sub-commands, parser returns argument map, deepest matched command
name, and a sub-spec passed for this command:

    4> Cmd =  #{arguments => [#{name => arg}]}.
    #{arguments => [#{name => arg}]}
    5> argparse:parse(["cmd", "value"], #{commands => #{"cmd" => Cmd}}).
    {#{arg => "value"},{"cmd",#{arguments => [#{name => arg}]}}}

### Commands
Command specification may contain following fields:
  * **commands** - sub-commands, name to nested command spec
  * **arguments** - list of argument specifications
  * **help** - string to generate usage information
  * **handler** - function accepting arguments map,
    automatically created by cli:run, unless specified by user
  * user-defined fields

Command is matched as a positional argument, taking precedence over other
positional arguments. For any positional argument found, parser will first
attempt to match a sub-command in currently evaluated command. If there is no
match, parser considers next argument as positional. Example:

    1> Cmd = #{
        commands => #{sub => #{}},
        arguments => [#{name => positional}]
    },
    2> parse(["arg", "sub"], Cmd) == parse(["arg", "sub"], Cmd).
    true

Command names cannot start with prefix character.

### Arguments
Every argument spec must have **name** field. Name defines key in the parsed
arguments map, similar to *dest* field of Python library. It is allowed to have
multiple arguments with the same name (option aliases):

    options => [
        #{name => foo, short => $f},
        #{name => foo, long => "-foo"},
        #{name => foo}
    ].

An argument can be:
* *optional*: matching command line arguments with prefix character
* *positional*: arguments not starting with a prefix

Negative number is considered positional argument, when **-** prefix is used, and
there is no no optional argument spec that has short or long form
defined as number (```#{name => one, short => $1}```).

Argument is *optional* when **short** or **long** field is defined. Short form may only
contain a single character, and long form may contain any string. A single prefix
character is automatically prepended to long forms, so for this example:

    #{name => myarg, short => $m, long => "-myarg"}

Argument ```myarg``` can be specified as ```-m``` or as ```--myarg```. Please not that it
is possible to have long forms with a single prefix character:

    #{name => kernel, long => "kernel"}

By default, optional arguments are not required and may be omitted from command line.
Positional arguments are required. It is possible to override this behaviour by
setting **required** field.

For every argument matched, parser may consume several following positional arguments
(not starting with a prefix). Analysis is based on **nargs**, **action** and **type** fields.
 * when nargs is **maybe** and next argument is positional, it gets consumed and produced
    as value, and if next argument starts with option prefix, **default** value is produced
    (when no default present, default value deduced from argument **type**)
 * when nargs is **{maybe, Term}**, and next argument starts with option prefix, Term is
    produced
 * when nargs is set to a positive integer, parser tries to consume exactly this
    number of positional arguments, and fails with an error if there is not enough.
    Produced value is a list.
 * when nargs is set to **nonempty_list**, parser consumes at least one following positional
    argument (fails if first argument is not positional), until it finds next optional
    argument. Produced value is a list.
 * when nargs is **list**, parser consumes positional arguments until it finds next
    optional. Produced value is a list.
 * when nargs is **all**, all remaining arguments (whether positional or not) are folded
    into single produced list.

When nargs is not specified, **action** determines how many arguments are consumed. For
store/append, single argument is consumed. Special handling is done for boolean **type**:
no argument is consumed, even for store/append. If it is necessary to consume an argument
for boolean type, use ```type => atom``` with ```choices => ["true", "false"]```.

Action can have following values:
 * **store** - replace value in the argument map (last value wins)
 * **append** - append value to existing list (when value is also a list, resulting
   value can be a list of lists)
 * **{store, Term}** - do not consume argument, replace current value with Term
 * **{append, Term}** - do not consume argument, append Term to list of existing values
 * **count** - do not consume any arguments, bump the counter - useful for bumping logging
   verbosity level, e.g. ```cmd -vvv```
 * **extend** - valid only for nargs set to list/nonempty_list/all/(pos_integer()), explodes
   this list and appends every single value to existing list, so result is just a list, not
   a list of lists

Arguments have **type** associated. Default type is string, taken unprocessed from command
line input.
 * int, {int, Limits} - where Limits is a list containing {min, Int}, {max, Int} tuples
 * float, {float, Limits} - same as int, but for floating point
 * string, {string, Regex}, {string, Regex, RegexOption} - string validated with Regex
 * binary, {binary, Regex}, {binary, Regex, RegexOption} - same as string, but of binary type
 * atom - existing atom (error when atom does not exist)
 * {atom, unsafe} - atom, creates new atoms when needed
 * {custom, Fun} - custom type conversion from string() into term()

An error is thrown when argument cannot be converted to required type, or does not pass validation.
If custom conversion function throws error(invalid_argument), exception is augmented with necessary
parser state information (any other exception passes through unchanged).

It is not allowed to store user-defined fields in arguments.

### Errors

Argparse throws exceptions of class error, and Reason tuple:

    {argparse, ArgParseError}

To get human-readable representation:

    try argparse:parse(Args, Command)
    catch error:{argparse, Reason} ->
        io:format(argparse:format_error(Reason, Command))
    end.


## Help/usage information
CLI framework prints usage and error messages automatically.

## Build
This project requires OTP-22 or above. Simple integration is available via Hex and
rebar3.

    {deps, [argparse]}.

## Expected features

To be implemented in 1.0.0:
* more usage printing options
* choices validation

To be considered after 1.0.0:
* abbreviated long forms
* mutual exclusion groups
* support templates in help lines
* shell auto-complete
* support for "--arg=value" form
* automatically generated negative boolean long forms "--no-XXXX"

## Changelog

Version 0.1.0:
 * First usable version (API is unstable yet)
