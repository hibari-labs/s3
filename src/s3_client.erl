%%%-------------------------------------------------------------------
%%% Copyright (c) 2007-2013 Hibari developers.  All rights reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%-------------------------------------------------------------------
-module(s3_client).
-include("s3.hrl").

-export([
         make_state/5,
         head_object/3,
         put_object/5,
         put_bucket/3,
         delete_bucket/2,
         delete_object/3,
         get_service/1,
         get_service_xml/1,
         get_object/3,
         get_bucket/2,
         get_bucket_xml/2
        ]).

-type state() :: #state{}.

%% --- external API ----------------------------------------
-spec make_state(string(), integer(), string(), string(),
                 atom()) -> state().
%% @spec make_state(string(),integer(),string(),string(),
%%              atom()) -> state()
%% @doc  make "state" record
make_state(Host, Port, Id, AuthKey, Style) ->
    #state{host=Host, port=Port, id=Id, auth_key=AuthKey,
           style=Style}.


%% === SERVICE================
-spec get_service(state()) -> {ok, {#owner{}, [#bucket{}]}}.
%% @spec get_service(state()) -> {ok, {#owner{}, [#bucket{}]}}
%% @doc send GET SERVICE request
get_service(State) ->
    {ok,XML} = get_service_xml(State),
    s3_utils:xml_service(XML).

-spec get_service_xml(state()) -> {ok, binary()}.
%% @spec get_service_xml(state()) -> {ok, string()}
%% @doc send GET SERVICE request(return raw xml string)
get_service_xml(State) ->
    Op = get,
    Host = State#state.host,
    Path = "/",
    AuthPath = "/",
    Val = "",
    ACL = undefined,
    do_req(Op, State, Host, Path, AuthPath, Val, ACL).



%% === OBJECT ================
-spec put_object(state(),string(),string(),binary(),term())
                -> ok.
%% @spec put_object(state(),string(),string(),binary(),term())
%%        ->    ok
%% @doc  send PUT OBJECT request
put_object(State, Bucket, Key, Val, ACL) ->
    do_object(put, State, Bucket, Key, Val, ACL).

-spec delete_object(state(),string(),string()) -> ok.
%% @spec delete_object(state(),string(),string()) -> ok
%% @doc send DELETE OBJECT request
delete_object(State, Bucket, Key) ->
    do_object(delete,State,Bucket,Key,undefined,undefined).

-spec get_object(state(),string(),string()) -> {ok, binary()}.
%% @spec get_object(state(),string(),string())
%%        ->    {ok, binary()}
%% @doc send GET OBJECT request
get_object(State, Bucket, Key) ->
    do_object(get, State, Bucket, Key, undefined, undefined).

-spec head_object(state(),string(),string())
                 -> {ok, binary()}.
%% @spec head_object(state(),string(),string())
%%        ->    {ok, binary()}
%% @doc send HEAD OBJECT request
head_object(State, Bucket, Key) ->
    do_object(head, State, Bucket, Key, undefined, undefined).


%% === BUKCET ================
-spec put_bucket(state(),string(),term()) -> ok.
%% @spec put_object(state(),string(),term()) -> ok
%% @doc  send PUT BUCKET request
put_bucket(State, Bucket, ACL) ->
    do_bucket(put, State, Bucket, ACL).

-spec delete_bucket(state(),string()) -> ok.
%% @spec delete_bucket(state(),string()) -> ok
%% @doc send DELETE BUCKET request
delete_bucket(State, Bucket) ->
    do_bucket(delete, State, Bucket,undefined).

-spec get_bucket(state(),string()) -> {ok,#list_bucket{}}.
%% @spec get_bucket(state(),string())
%%         -> {ok, #list_bucket{}}
%% @doc send GET BUCKET request
get_bucket(State, Bucket) ->
    {ok, XML} = get_bucket_xml(State, Bucket),
    s3_utils:xml_list_bucket(XML).

-spec get_bucket_xml(state(),string()) -> {ok, binary()}.
%% @spec get_bucket_xml(state(),string()) -> {ok, #bucket{}}
%% @doc send GET BUCKET request(return raw xml string)
get_bucket_xml(State, Bucket) ->
    do_bucket(get, State, Bucket, undefined).

%%--- internal ---------------------------------------------
do_object(Op, State, Bucket, Key, Val, _ACL) ->
    #state{host=Host0, style=Style} = State,
    {Host, Path, AuthPath} =
        case Style of
            ?S3_PATH_STYLE ->
                {Host0, "/"++Bucket++"/"++Key,
                 "/"++Bucket++"/"++Key};
            ?S3_VIRTUAL_HOSTED_STYLE ->
                {Bucket++"."++Host0, "/"++Key,
                 "/"++Bucket++"/"++Key}
        end,
    do_req(Op, State, Host, Path, AuthPath, Val, _ACL).

do_bucket(Op, State, Bucket, _ACL) ->
    #state{host=Host0, style=Style} = State,
    {Host, Path, AuthPath} =
        case Style of
            ?S3_PATH_STYLE ->
                {Host0, "/"++Bucket, "/"++Bucket};
            ?S3_VIRTUAL_HOSTED_STYLE ->
                {Bucket++"."++Host0, "/",
                 "/"++Bucket++"/"}
        end,
    Val = "",
    do_req(Op, State, Host, Path, AuthPath, Val, _ACL).

do_req(Op, State, Host, Path, AuthPath, Val, _ACL) ->
    #state{port=Port,
           id=Id, auth_key=AuthKey} = State,
    Date = httpd_util:rfc1123_date(),
    Header0 = [{"Host",Host},
               {"Connection","close"},
               {"Content-type","text/plain"},
               {"Date",Date}],
    Header =
        s3_utils:make_auth(Op,Id,AuthKey,AuthPath,Header0),
    URL = "http://"++Host++":"++integer_to_list(Port)++Path,
    do_client(Op, URL, Header, "text/plain", Val).

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



fix_resp(head, {ok, {_Resp,Header,_Body}}) ->
    {ok, Header};
fix_resp(get, {ok, {_Resp,_Header,Body}}) ->
    {ok, Body};
fix_resp(delete, {ok,{{_,204,_},_,_}}) ->
    ok;
fix_resp(delete, {ok,{{_,404,_},_,_}}) ->
    key_not_exist;
fix_resp(_, {ok,{{_,200,_},_,_}}) ->
    ok;
fix_resp(_, Err) ->
    Err.


