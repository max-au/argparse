# argparse reference

Parser operates with *arguments* and *commands*, organised in a hierarchy. It is possible
to define multiple commands, or none. Parsing always starts with root *command*,
named after ```init:get_argument(progname)```. Empty command produces empty argument map:

    1> parse("", #{}).
    #{}

## Options specification

It's possible to override program name using **progname** option:

    2> io:format(argparse:help(#{}, #{progname => "readme"})).
    usage: readme

To override default optional argument prefix (**-**), use **prefixes** option:

    3> argparse:parse(["+sbwt"], #{arguments => [#{name => mode, short => $s}]}, #{prefixes => "+"}).
    #{mode => "bwt"}

## Validation, help & usage information

Function ```validate/1``` may be used to validate command with all sub-commands
and options without actual parsing done.

    4> argparse:validate(#{arguments => [#{short => $4}]}).
    ** exception error: {argparse,{invalid_option,["erl"],
        [],name,"argument must be a map, and specify 'name'"}}

Human-readable errors and usage information is accessible via ```help/2``` and ```format_error/1,2```.

## Return value

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

## Command specification
Command specification may contain following fields:
  * **commands** - sub-commands, name to nested command spec
  * **arguments** - list of argument specifications
  * **help** - string to generate usage information
  * **handler** - function expected to process this command, or *optional* atom
  * user-defined fields

Missing handler field is automatically populated by CLI framework, when a module
exporting ```cli/0``` also exports function function with arity 1, named
after command:

    cli() -> #{commands => #{"run" => #{...}}}.
    
    run(ArgMap) -> ....

If command contains sub-commands, and handler is not present, it is an error
if arguments supplied to ```parse``` do not select one of the sub-commands. Set
handler to *optional* to not require sub-command selected.

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

## Argument specification
Every argument spec must have **name** field. Name defines key in the parsed
arguments map, similar to *dest* field of Python library. It is allowed to have
multiple arguments with the same name (option aliases):
```erlang
    options => [
        #{name => foo, short => $f},
        #{name => foo, long => "-foo"},
        #{name => foo}
    ].
```

An argument can be:
* *optional*: matching command line arguments with prefix character
* *positional*: arguments not starting with a prefix

Negative number is considered positional argument, when **-** prefix is used, and
there is no no optional argument spec that has short or long form
defined as number (```#{name => one, short => $1}```).

Argument is *optional* when **short** or **long** field is defined. Short form may only
contain a single character, and long form may contain any string. A single prefix
character is automatically prepended to long forms, so for this example:
```erlang
    #{name => myarg, short => $m, long => "-myarg"}
```
Argument ```myarg``` can be specified as ```-m``` or as ```--myarg```. Please note that it
is possible to have long forms with a single prefix character:
```erlang
    #{name => kernel, long => "kernel"}
```
By default, optional arguments are not required and may be omitted from command line.
Positional arguments are required. It is possible to override this behaviour by
setting **required** field.

It is supported to specify long-form value using this syntax:
```shell
    mycli --arg=value
```
It is treated the same way as `mycli --arg value`.

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

For int, float, string, binary and atom, it is also possible to specify list of
choices that will pass validation:
```erlang
    #{name => choices, short => $c, type => {string, ["one", "two", "three"]}}
```
Custom function may throw ```erlang:error(invalid_argument)```, to utilise built-in
validation information. If any other exception is raised, it is passed with no conversion.

An error is thrown when argument cannot be converted to required type, or does not pass validation.
If custom conversion function throws error(invalid_argument), exception is augmented with necessary
parser state information (any other exception passes through unchanged).

It is not allowed to store user-defined fields in arguments.

## Errors

Argparse throws exceptions of class error, and Reason tuple:
```erlang
    {argparse, ArgParseError}
```
To get human-readable representation:
```erlang
    try argparse:parse(Args, Command)
    catch error:{argparse, Reason} ->
        io:format(argparse:format_error(Reason, Command, #{}))
    end.
```

## Help templates

It is possible to override help text generated for arguments. By default,
options are formatted with "help text, type, default", e.g.:

    crawl [-s <shard>...] [-z <last>] [-c <choice>]

    Optional arguments:
      -s     initial shards, int
      -z     last shard, between, 100 < int < 200, 150
      -c     tough choice, choice: 1, 2, 3

It is possible to override the description, and print it this way:

    crawl [-s SHARD] [-c CHOICE]

    Optional arguments:
      -s     initial number, int, with a default value of 0
      -z     150, number of last shard, unless overridden
      -c     custom parameter, to choose from 1, 2 or 3

Example:
```erlang
  #{
    arguments => [
      #{
        name => shard,
        default => 0,
        help => {"[-s SHARD]", ["initial number, ", type, " with a default value of ", default]}}
    ]}
```
First element of the tuple replaces `[-s <shard>]` with `[-s SHARD]` in command line example, and
second defines detailed help template.
