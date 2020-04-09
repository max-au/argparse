%%-------------------------------------------------------------------
%%% @copyright (c) Maxim Fedorov <maximfca@gmail.com>
%%% @doc Example from argparse framework
-module(multi_string).
-author("maximfca@gmail.com").

%% API
-export([
    cli/0
]).

-behaviour(cli).

cli() ->
    #{
        commands => #{
            "lowercase" => #{
                help => "lowercase strings",
                handler => fun (#{str := Str}) ->
                    [io:format("Lowercase: ~s~n", [string:lowercase(S)]) || S <- Str]
                           end,
                arguments => [
                    #{name => str, nargs => nonempty_list, type => string, help => "strings to lowercase"}
                ]
            },
            "lexemes" => #{
                handler => {fun(Str, Sep) -> io:format("Lexemes: ~p~n", [string:lexemes(Str, Sep)]) end, undefined},
                arguments => [
                    #{name => str, type => string, help => "string to split"},
                    #{name => separator, short => $s, type => string, default => " ", help => "separator to use"}
                ]
            }
        }
    }.
