-module(tt_adapter).

-author('liuhao@worktile.com').

-behavior(gen_mod).
-behavior(gen_server).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").
-include("ejabberd_http.hrl").

-define(RESP_SUCCESS, {obj, [{"code": 0}]}).

-define(PROCNAME, ejabberd_mod_adapter).

-define(SYSADMIN, <<"sysadmin">>).

-define(SUPERVISOR, ejabberd_sup).

-export([process/2, construct_event/5]).

-export([start_link/2]).

%% gen_mod callbacks
-export([start/2, stop/1]).

%% gen_server callbacks
-export([init/1, terminate/2, handle_call/3,
	 handle_cast/2, handle_info/2, code_change/3]).

-record(state, {host = <<"">>, token = <<"this is a feed server!">>}).

start_link(Host, Opts) ->
    Proc = get_proc_name(Host),
    gen_server:start_link({local, Proc}, ?MODULE,
			  [Host, Opts], []).

%%====================================================================
%% gen_mod callbacks
%%====================================================================
start(Host, Opts) ->
    Proc = get_proc_name(Host),
    AdapSpec = {Proc, {?MODULE, start_link, [Host, Opts]},
		permanent, 2000, worker, [?MODULE]},
    supervisor:start_child(?SUPERVISOR, AdapSpec).

stop(Host) ->
    Proc = get_proc_name(Host),
    gen_server:call(Proc, stop),
    supervisor:terminate_child(?SUPERVISOR, Proc),
    supervisor:delete_child(?SUPERVISOR, Proc).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Host, Opts]) ->
    Token = gen_mod:get_opt(token, Opts, fun(A) -> A end),
    {ok, #state{host = Host, token = Token}}.

terminate(_Reason, #state{host = _Host}) ->
    ok.

handle_call({pubmsg, ReqToken, Data}, _From, #state{host = Host, token = Token} = State) ->
    io:format("req token:~p , my token : ~p ~n", [ReqToken, Token]),
    case ReqToken of
    	 Token -> 
	    FromJid = jlib:make_jid(?SYSADMIN, Host, <<"">>),
	    Users = ejabberd_sm:get_vh_session_list(Host),
	    lists:foreach(fun(To) ->
				  Packet = construct_event(FromJid, To, Data),
				  ejabberd_router:route(FromJid, ToJid, Packet);				  
			     end, Users),
	    {reply, {ok, published}, State};
	_ ->
	    ?ERROR_MSG("invalid token request!", []),
	    {reply, {error, "invalid token"}, State}
    end;
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Req, _From, State) ->
    {reply, {error, badarg}, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%======================================================================
%% http callback
%%======================================================================
process([<<"pub">>], Request) ->
    QueryStr = Request#request.q,
    Data = Request#request.data,
    Proc = get_proc_name(Request#request.host),
    Headers = Request#request.headers,
    {<<"Tt-Token">>, Token} = lists:keyfind(<<"Tt-Token">>, 1, Headers),
    ?INFO_MSG("recev headers: ~p, q:~p, d:~p, process by: ~p ~n", [Headers, QueryStr, Data, Proc]),
    try gen_server:call(Proc, {pubmsg, Token, Data}) of
	{ok, _} ->
	    "{\"code\":0}";
	{error, _Why} ->
	    "{\"code\":401}"
    catch
	exit:{noproc, _} ->
	    ?ERROR_MSG("Received an HTTP request with Host ~p, but couldn't find the related "
		       "ejabberd virtual host", [Request#request.host]),
	    ejabberd_web:error(not_found)
    end;
process(_, _Request) ->
    ejabberd_web:error(not_found).

construct_event(F, T, Event) ->
    Id = randoms:get_string(),
    Attrs = [{<<"xml:lang">>, <<"en">>},
	     {<<"type">>, <<"headline">>},
	     {<<"from">>, jlib:jid_to_string(F)},
	     {<<"to">>, jlib:jid_to_string(T)},
	     {<<"id">>, Id}],
    Body = {xmlel, <<"body">>, [], [{xmlcdata, Event}]},
    Blank = {xmlcdata, <<"\n">>},
    {xmlel, <<"message">>, Attrs, [Body, Blank]}.

get_proc_name(Host) -> gen_mod:get_module_proc(Host, ?PROCNAME).

json_to_model(JsonList) ->
    case lists:sort(JsonList) of
	[{<<"body">>, {Body}},{<<"client">>, Client},{<<"from">>, {From}},{<<"style">>, Style}, {<<"to">>, {To}}, {<<"type">>, Type}] ->
	    {ToType, ToId, To2} = parse_to(To),
	    {FromType, FromId, From2} = parse_from(From),
	    M = {
	      from, From2,
	      to, To2,
	      type, Type,
	      body, parse_body(Body),
	      is_deleted, 0,
	      style, Style,
	      client, Client,
	      created_at, jlib:get_now_utc()
	     },
	    {ToType, ToId, FromType, FromId,  M}; 
	_ ->
	    undefined
    end.

parse_body(Body) ->
    case lists:keytake(<<"entity">>, 1, Body) of
	{value, {<<"entity">>, {E}}, S} when is_list(E)->
	    {<<"id">>, Id} = lists:keyfind(<<"id">>, 1,E),
	    S2 = S ++ [{<<"entity">>, Id}],
	    jiffy:encode({S2});
	_ ->
	    jiffy:encode({Body})
    end.
    
parse_from(From) ->
    case lists:keytake(<<"type">>, 1, From) of
	{value, {<<"type">>, V}, From2} ->
	    case lists:keytake(<<"uid">>, 1, From2) of
		{value, {<<"uid">>, U}, _} ->
		    {V, U, list_to_tuple([type, V, uid, U])};
		_ ->
		    {undefined, undefined, From}
	    end;
	_ ->
	    {undefined, undefined, From}
    end.

parse_to(To) ->
    case lists:keytake(<<"type">>, 1, To) of
	{value, {<<"type">>, V}, To2} ->
	    case lists:keytake(<<"id">>, 1, To2) of
		{value, {<<"id">>, U}, _} ->
		    {V, U, list_to_tuple([type, V, id, U])};
		_ ->
		    {undefined, undefined, To}
	    end;
	_ ->
	    {undefined, undefined, To}
    end.

get_members() ->    
    
			
    
			


