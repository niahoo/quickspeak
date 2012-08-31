-module(quickspeak).


-export([start/0,client/0]).

-include("../include/quickspeak.hrl").

start() -> application:start(quickspeak).

client() ->
    timer:sleep(500),
    {ok, [[Nick]]} = init:get_argument(nickname),
    % BinPrompt = list_to_binary(Nick ++">"),
    client(#clstate{nickname=Nick}).

client(State) ->
    Content = io:get_line(""),
    case Content
     of "\n" ->
            io:format(State#clstate.nickname ++"> "),
            client(State)
      ; "/" ++ Command ->
            {ok, NewState} = qs_command:exec(Command, State),
            io:format(NewState#clstate.nickname ++"> "),
            client(NewState)
      ; Msg ->
            qs_filemon:write(State#clstate.nickname ++"> " ++ Msg),
            qs_filemon:check(),
            client(State)
    end.




