-module(qs_command).

-include("../include/quickspeak.hrl").

-export([exec/2]).



exec(Command, State) ->
    {Fun, Args} = parse(Command),
    exec(Fun, Args, State).

exec("quit",[],_)  ->
    io:format("Fermeture du programme"),
    init:stop(),
    exitloop();

exec("clear",[],State) ->
    ok = qs_filemon:clear(),
    {ok, State};

exec("nick",[Nick],State) when is_list(Nick) -> {ok, State#clstate{nickname=Nick}};



%% Par défaut si la commande est inconnue c'est ok.
exec(Command,Args,State) -> 
    error_logger:error_msg("Commande non reconnue. ~n\tCommande: ~p~n\tArguments: ~p~n", [Command,Args]),
    {ok, State}.



parse(Command) -> 
    Toks = string:tokens(Command," \n\t\s"),
    case Toks
     of [] -> {ok, ok}
      ; [Fun|Args] -> {Fun, Args}
    end.





exitloop() -> io:format("."), timer:sleep(50), exitloop().