-module(ejabberd_auth_redis).

-author('liuhao@worktile.com').

-behaviour(ejabberd_auth).

%% External exports
-export([start/1, set_password/3, check_password/3,
	 check_password/5, try_register/3,
	 dirty_get_registered_users/0, get_vh_registered_users/1,
	 get_vh_registered_users/2,
	 get_vh_registered_users_number/1,
	 get_vh_registered_users_number/2, get_password/2,
	 get_password_s/2, is_user_exists/2, remove_user/2,
	 remove_user/3, store_type/0,
	 plain_password_required/0]).

-include("ejabberd.hrl").
-include("logger.hrl").

-define(USER_TOKEN, <<"lc_user_token:">>).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(_Host) -> ok.

plain_password_required() -> false.

store_type() -> plain.

%% @spec (User, Server, Password) -> true | false | {error, Error}
check_password(User, _Server, Password) ->
    LUser = jlib:nodeprep(User),
    %% LServer = jlib:nameprep(Server),
    Key = <<?USER_TOKEN/binary, LUser/binary>>,
    case ejabberd_redis:get(Key) of
	none -> 
	    false;
        Password when is_binary(Password) ->
            Password /= <<"">>;
        _ ->
            false
    end.

check_password(User, _Server, Password, Digest,
	       DigestGen) ->
    LUser = jlib:nodeprep(User),
    %% LServer = jlib:nameprep(Server),
    Key = <<?USER_TOKEN/binary, LUser/binary>>,
    case ejabberd_redis:get(Key) of
	none ->
	    false;
	Passwd when is_binary(Passwd) ->
	    DigRes = if Digest /= <<"">> ->
			     Digest == DigestGen(Passwd);
			true -> false
		     end,
	    if DigRes -> true;
	       true -> (Passwd == Password) and (Password /= <<"">>)
	    end;
	_ -> false
    end.

set_password(_User, _Server, _Password) ->
    ok.

try_register(_User, _Server, _PasswordList) ->
    {error, invalid_jid}.

dirty_get_registered_users() ->
    [].

get_vh_registered_users(_Server) ->
    [].

get_vh_registered_users(Server, _) ->
    get_vh_registered_users(Server).

get_vh_registered_users_number(_Server) ->
    0.

get_vh_registered_users_number(Server, _) ->
    get_vh_registered_users_number(Server).

get_password(User, _Server) ->
    LUser = jlib:nodeprep(User),
    %% LServer = jlib:nameprep(Server),
    Key = <<?USER_TOKEN/binary, LUser/binary>>,
    case ejabberd_redis:get(Key) of
	none ->
	    false;
	Passwd when is_binary(Passwd) ->
           Passwd;
	_ -> false
    end.

get_password_s(User, Server) ->
    get_password(User, Server).

is_user_exists(_User, _Server) ->
    true.

remove_user(_User, _Server) ->
    ok.

remove_user(_User, _Server, _Password) ->
    ok.

