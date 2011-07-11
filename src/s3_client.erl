-module(s3_client).
-include("s3.hrl").

-export([
	 put_object/5,
	 put_bucket/3,
	 delete_bucket/3,
	 delete_object/4,
	 get_object/4,
	 get_bucket/2
	]).

%% --- external API ----------------------------------------
%% @spec (#state(), string(), string(), binary(), term())
%%         -> ok|term()
%% @doc  send PUT OBJECT request
put_object(State, Bucket, Key, Val, ACL) ->
    do_object(put, State, Bucket, Key, Val, ACL).
delete_object(State, Bucket, Key, ACL) ->
    do_object(delete, State, Bucket, Key, undefined, ACL).
get_object(State, Bucket, Key, ACL) ->
    do_object(get, State, Bucket, Key, undefined, ACL).

put_bucket(State, Bucket, ACL) ->
    do_bucket(put, State, Bucket, ACL).
delete_bucket(State, Bucket, ACL) ->
    do_bucket(delete, State, Bucket, ACL).
get_bucket(State, Bucket) ->
    do_bucket(get, State, Bucket, undefined).

%%--- internal ---------------------------------------------
do_object(Op, State, Bucket, Key, Val, _ACL) ->
    #state{host=Host, port=Port,
	   id=Id, auth_key=AuthKey} = State,
    Header0 = [{"Host",Host},{"connection","close"}],
    Header = make_auth(Id, Header0, AuthKey),
    URL = "http://"++Host++":"++integer_to_list(Port)
	++"/"++Bucket++"/"++Key,
    do_client(Op, URL, Header, "text/plain", Val).

do_bucket(Op, State, Bucket, _ACL) ->
    #state{host=Host, port=Port,
	   id=Id, auth_key=AuthKey} = State,
    Header0 = [{"Host",Host},{"connection","close"}],
    Header = make_auth(Id, Header0, AuthKey),
    URL = "http://"++Host++":"++integer_to_list(Port)
	++"/"++Bucket,
    do_client(Op, URL, Header, "text/plain", "").

do_client(Op, URL, Header, CType, Body) ->
    Req = case Op of
	      put ->
		  {URL, Header, CType, Body};
	      _ ->
		  {URL, Header}
	  end,
		      
    Resp = httpc:request(Op, Req, [],
			 [{body_format, binary}]),
    fix_resp(Op, Resp).



fix_resp(get, {ok, {_Resp,_Header,Body}}) ->
    {ok, Body};
fix_resp(_, {ok,_}) ->
    ok;
fix_resp(_, Err) ->
    Err.
    
make_auth(Id, H0, _Key) ->
    Sig = "todo",
    [{"Authorization","AWS "++ Id++":"++Sig}|H0].
    
