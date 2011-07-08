-module(s3_client).
-include("s3.hrl").

-export([
	 add_user/1,
	 put_bucket/3,
	 delete_bucket/3,
	 get_bucket/2]).

add_user(_State) ->
    ok.

put_bucket(State, Bucket, ACL) ->
    do_bucket(put, State, Bucket, ACL).

delete_bucket(State, Bucket, ACL) ->
    do_bucket(delete, State, Bucket, ACL).

get_bucket(State, Bucket) ->
    do_bucket(get, State, Bucket, undefined).

do_bucket(Op, State, Bucket, _ACL) ->
    #state{host=Host, port=Port,
	   id=Id, auth_key=AuthKey} = State,
    Header0 = [{"Host",Host},{"connection","close"}],
    Header = make_auth(Id, Header0, AuthKey),
    URL = "http://"++Host++":"++integer_to_list(Port)
	++"/"++Bucket,
    Req = case Op of
	      put ->
		  {URL, Header,"text/plain", ""};
	      _ ->
		  {URL, Header}
	  end,
		      
    Resp = httpc:request(Op, Req, [], []),
    fix_resp(Op, Resp).



%%--- internal
fix_resp(get, X) ->
    X;
fix_resp(_, {ok,_}) ->
    ok;
fix_resp(_, Err) ->
    Err.
    

make_auth(Id, H0, _Key) ->
    Sig = "todo",
    [{"Authorization","AWS "++ Id++":"++Sig}|H0].
    
