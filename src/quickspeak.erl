-module(quickspeak).


-export([start/0,client/0]).
-record(state,{nickname}).


start() -> application:start(quickspeak).
           
client() -> 
    timer:sleep(500),
    {ok, [[Nick]]} = init:get_argument(nickname),
    % BinPrompt = list_to_binary(Nick ++">"),
    client(#state{nickname=Nick}).

client(State) ->
    Content = io:get_line(""),
    case Content
     of "\n" -> 
            client(State)
      ; Any ->
            qs_filemon:write(State#state.nickname ++">" ++ Content),
            qs_filemon:check(),
            client(State)
    end.
    
    


