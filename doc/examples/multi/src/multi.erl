%%-------------------------------------------------------------------
%%% @copyright (c) Maxim Fedorov <maximfca@gmail.com>
%%% @doc Example from argparse framework
-module(multi).
-author("maximfca@gmail.com").

%% API
-export([
    main/1
]).

main(Args) ->
    multi_math:module_info(),
    multi_string:module_info(),
    %% ensure code loaded
    %% normally it is done by application starting up supervision tree,
    %%  yet this example is kept simple on purpose
    cli:run(Args).
