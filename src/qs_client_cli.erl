-module(qs_client_cli).
-behaviour(gen_server).

-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-define(BASE_TIMEOUT, 30).


-record(state, {filename,nickname}).

start_link(Filename,Nick) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Filename,Nick], []).
    

init([Filename,Nick]) ->    
    io:format("Filename Client : ~p~n",[Filename]),
    {ok, #state{filename=Filename,nickname=Nick},0}.


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State, ?BASE_TIMEOUT}.

handle_cast(_Msg, State) ->
    {noreply, State, ?BASE_TIMEOUT}.

handle_info(timeout, State) ->
    Content = ssh_io:read_line(State#state.nickname ++">"),
    io:format("Content: ~p" ,[Content]),
    qs_filemon:write(Content),
    {noreply, State, ?BASE_TIMEOUT};

handle_info(_Info, State) ->
    {noreply, State, ?BASE_TIMEOUT}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State, ?BASE_TIMEOUT}.