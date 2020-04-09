%%-------------------------------------------------------------------
%%% @copyright (c) Maxim Fedorov <maximfca@gmail.com>
%%% @doc Example from argparse framework
-module(multi_math).
-author("maximfca@gmail.com").

%% API
-export([
    cli/0,
    sum/1,
    cos/1
]).

-behaviour(cli).

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
                handler => fun (#{left := Left, right := Right}) -> Left * Right end,
                arguments => [
                    #{name => left, type => int},
                    #{name => right, type => int}
                ]
            }
        }
    }.

sum(#{num := Nums}) ->
    lists:sum(Nums).

cos(#{in := In}) ->
    math:cos(In).
