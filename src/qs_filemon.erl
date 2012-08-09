-module(qs_filemon).

-behaviour(gen_server).
-include_lib("kernel/include/file.hrl").
%% API
-export([
     start_link/2
    ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).


-export([write/1,check/0]).

-define(SERVER, ?MODULE). 
%% Timeout pour check le fichier de chat, en milisecondes
-define(BASE_TIMEOUT, 3000).

-record(state, {filename,previous_size=0,nickname}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(HabID) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Filename,Nick) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Filename,Nick], []).


write(Data) when is_list(Data) -> write(list_to_binary(Data)) ;
write(Data) ->
    gen_server:call(?SERVER, {data,Data}).

check() ->
    gen_server:call(?SERVER, check).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Filename,Nick]) ->
    % error_logger:info_msg("Loading client on file :~p~n", [Filename]),
    {ok, #state{filename=Filename,nickname=Nick}, 0}. 

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_call(check,  _From,State) ->
    {reply,ok, State, 0}; %% instant timeout

handle_call({data, Data},  _From, State) ->
    file:write_file(State#state.filename,Data,[append]),
    {reply,ok, State, ?BASE_TIMEOUT};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State, ?BASE_TIMEOUT}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_cast(_Msg, State) ->
    {noreply, State, ?BASE_TIMEOUT}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, State) ->
    % io:format("timeout reçu~n!"),
    %% à chaque timeout on regarde si le fichier à été mis à jour
    {IsUpd, NewState} = x_check_upd(State),
    case IsUpd
     of true -> %% si le fichier est updaté on envoir simplement le
                %% contenu dans le shell
        {ok, Bin} = file:read_file(State#state.filename),
        x_clear(),
        ok = io:format("~s~n", [Bin]),
        ok = io:format("~s",[State#state.nickname ++ "> "])
      ; _ -> osef
    end,
    {noreply, NewState, ?BASE_TIMEOUT};

handle_info(_Info, State) ->
    {noreply, State, ?BASE_TIMEOUT}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State, ?BASE_TIMEOUT}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
x_check_upd(State) ->
    %% Pour savoir si le fichier a été mis à jour, il suffit de
    %% regarder si sa taille a augmenté
    Filename = State#state.filename,
    x_ensure_file_exists(Filename),
    {ok, Fileinfo} = file:read_file_info(Filename),
    CurrentSize = Fileinfo#file_info.size,
    PreviousSize = State#state.previous_size,
    case CurrentSize =/= PreviousSize % si taille différente alors MAJ == true
        of true -> {true, State#state{previous_size=CurrentSize}}
         ; false -> {false, State}
    end.
     



x_ensure_file_exists(Filename) ->
    case filelib:is_regular(Filename)
     of false ->
            ok = x_touch(Filename),
            true = filelib:is_regular(Filename),
            ok
      ; true -> 
            ok
    end.

x_touch(Filename) ->
    error_logger:info_msg("Fichier créé, appuyez sur Entrée."),
    ok = file:write_file(Filename, <<>>),
    ok.

x_clear() -> 
    Rows = case io:rows()
        of {ok, Int} -> Int
         ; _ -> 25 %%@todo handle error
    end,
    io:put_chars(lists:flatten(lists:duplicate(Rows-3,$\n))).
