-module(conf_reader).

-export([main/1]).

main(Args) ->
    try
        #{file := File} = Parsed = argparse:parse(Args, cli()),
        {ok, Terms} = file:consult(File),
        Filtered = filter(key, Parsed, filter(app, Parsed, Terms)),
        io:format("~tp.~n", [Filtered])
    catch
        error:{argparse, Reason} ->
            io:format("error: ~s~n", [argparse:format_error(Reason)])
    end.

filter(ArgKey, Parsed, Terms) when is_map_key(ArgKey, Parsed) ->
    ArgVal = maps:get(ArgKey, Parsed),
    {_, Val} = lists:keyfind(ArgVal, 1, Terms),
    Val;
filter(_ArgKey, _Parsed, Terms) ->
    Terms.

%% parser specification
cli() ->
    #{arguments => [
        #{name => file},
        #{name => app, short => $a, type => {atom, unsafe}},
        #{name => key, short => $k, type => {atom, unsafe}}
    ]}.
