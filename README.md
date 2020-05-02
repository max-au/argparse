# argparse: command line parser for Erlang

A simple framework to create complex CLI. Inspired by Python argparse.

Follows conventions of  Unix Utility Argument Syntax.

    argparse [-abcDxyz][-p arg][operand]

## Argument parser
Converts list of strings (command line) into an argument map,and a
command path; see [argparse reference](doc/ARGPARSE.md) for detailed description.

## CLI Framework
Make a step beyond parser, and export existing Erlang functions: [cli reference](doc/CLI.md)


## Basic [example](doc/examples/escript/simple)

CLI framework is naturally suitable for building small escript-based apps:

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

The example above does not have sub-commands, and implements optional cli/1
callback, that serves as an entry point with parsed arguments. Help options are
added automatically:

    $ ./erm --help
    usage: erm [-fr] <dir>

    Optional arguments:
      dir
      -r  recursive, [false]
      -f  force, [false]


## Calc: CLI with [multiple commands](doc/examples/escript/calc)

Calculator implements several commands, with sub-commands available. Full
source code here: [doc/examples/escript/calc](doc/examples/escript/calc)

Command definitions:

    cli() ->
        #{
            commands => #{
                "sum" => #{
                    arguments => [
                        #{name => num, nargs => nonempty_list, type => int, help => "Numbers to sum"}
                    ]
                },
                "math" => #{
                    commands => #{
                        "sin" => #{handler => {math, sin, undefined}},
                        "cos" => #{},
                        "tan" => #{handler => {math, tan, undefined}}
                    },
                    arguments => [
                        #{name => in, type => float, help => "Input value"}
                    ]
                },
                "mul" => #{
                    arguments => [
                        #{name => left, type => int},
                        #{name => right, type => int}
                    ]
                }
            }
        }.

Calculator provides "sum" command that prints a sum of integer numbers:

    $ ./calc sum 1 2 3
    6

Math sub-commands provide trigonometric functions:

    $ ./calc math cos 1.4
    0.16996714290024104
    $ ./calc math sin 1.4
    0.9854497299884601

## Complex applications

CLI framework is capable of handling releases containing hundreds of modules
implementing cli behaviour. Commands may be exported from multiple modules and
applications. cli framework makes best efforts to merge commands exported,
format usage output and error messages.

See example: [doc/examples/multi](doc/examples/multi)

This example contains two modules, multi_math.erl and multi_string.erl.

Use ```rebar3 escriptize``` to build the application. Try various commands,
e.g. ```./multi math cos 1.0```, or ```./multi string lexemes 1+2+3+4 -s +```
to get a feeling!

## Argument [parser alone](doc/examples/escript/erm)

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

## Help and usage information
CLI framework automatically prints usage, if command line parser reports an
error. An attempt is made to guess most relevant command.


## Build
This project requires OTP-22 or above. Simple integration is available via Hex and
rebar3.

    {deps, [argparse]}.

## Known incompatibilities:
  * boolean flag (option), automatically using {store, true}
  * all positional arguments are required by default (even 'maybe')
  * first-class (sub) commands, slightly differently from argparse
  * implicit --help/-h is not a part of argparse (but implemented in cli)

Commands vs. positional arguments: command always takes precedence
over positional argument.
Commands form exclusive groups, e.g. only one command can
be followed at a time.

Kinds of arguments supported:
 * command (priority positional argument) : ectl {crawler|reader|writer}
 * command, and sub-command:                ectl crawler {start|stop|check}
 * positional argument (required):          ectl <arg1> <arg2>
 * positional argument (with default):      ectl [<arg1>]
 * boolean flag:              ectl [-rf]
 * required flag:             ectl -r
 * short optional argument:   ectl [-i <int>]
 * short optional:            ectl [-i [<int>]]
 * required short option:     ectl -i <int>
 * long option flag:          ectl [--foo]
 * long optional argument:    ectl [--foo <arg>]
 * required long:             ectl --foo <arg>
 * list of arguments:         ectl <arg>, ...

## Expected features

To be considered after 1.0.0:
* search for commands and arguments (mini-man)
* abbreviated long forms
* mutual exclusion groups
* support templates in help lines
* handler hooks (global options support)
* shell auto-complete
* support for "--arg=value" form
* automatically generated negative boolean long forms "--no-XXXX"

## Changelog

Version 1.0.1:
 * First public release

Version 0.1.0:
 * First usable version (API is unstable yet)
