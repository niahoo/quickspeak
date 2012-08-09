-module(quickspeak).


-export([start/0,client/0]).

start() -> application:start(quickspeak).
           
client() -> 
    timer:sleep(500),
    {ok, [[Nick]]} = init:get_argument(nickname),
    % BinPrompt = list_to_binary(Nick ++">"),
    client(Nick).

client(Nick) ->
    Content = io:get_line(""),
    qs_filemon:write(Nick ++">" ++ Content),
    qs_filemon:check(),
    client(Nick).


