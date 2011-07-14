%%%---------------------------------------------------------
%%% Copyright (c) 2008-2011 Gemini Mobile Technologies, Inc.  All rights reserved.
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
%%%
%%% File    : gmt_time_tests.erl
%%% Purpose : GMT time test suite
%%%---------------------------------------------------------

-module(s3_client_tests).
-include("s3.hrl").
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(MUT, s3_client). % Module Under Test (a.k.a. DUT)
-define(ATM, ?MODULE). % Automatic Test Module (a.k.a. ATE)

-define(FIELD_KEY, "x-amz-key").
-define(FIELD_KEYID, "x-amz-key-id").

all_tests_test_() ->
    application:start(inets),
    {State, ServerType} = server_info(),
    all_tests_(State, ServerType,
	       fun test_setup/0,
	       fun test_teardown/1).

all_tests_(State,ServerType,Setup,Teardown) ->
    {setup,
     Setup,
     Teardown,
     [
      ?_test(test_000(State,ServerType)),
      ?_test(test_001(State,ServerType)),
      ?_test(test_002(State,ServerType)),
      ?_test(test_003(State,ServerType)),
      ?_test(test_zzz(State,ServerType))
     ]
    }.

test_setup() ->
    ok.

test_teardown(_) ->
    ok.

server_info() ->
    Env = os:getenv("S3_TEST_SERVER"),
    if Env==false ->
	    {undefined, undefiend};
       true ->
	    server_info0(string:tokens(Env, ":"))
    end.

server_info0(["hibari"|_]) ->
    %% provisioning
    {ok, Id, AuthKey} = add_user("test_user000"),
    Style = ?S3_PATH_STYLE,
    %% assuming s3 server is running on port 23580
    {?MUT:make_state("localhost",23580,Id,AuthKey,Style),
     hibari};

server_info0([Type,Host,P0,Id,AuthKey]) when
      Type=="cloudian" orelse Type=="amz" ->
    Port = list_to_integer(P0),
    Style = ?S3_VIRTUAL_HOSTED_STYLE,
    %% Style = ?S3_PATH_STYLE,
    {?MUT:make_state(Host,Port,Id,AuthKey,Style),
     list_to_atom(Type)};

server_info0(_) ->
    {undefined, undefined}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Test Cases
%%


%% --- put, get and delete buckets
test_000(undefined,_) ->
    ok;
test_000(State,_) ->
    ACL = undefined,
    Bucket = "bucket000",
    ok = ?MUT:delete_bucket(State, Bucket),
    ok = ?MUT:put_bucket(State, Bucket, ACL),
    {ok,_R} = ?MUT:get_bucket(State, Bucket),
    ok = ?MUT:delete_bucket(State, Bucket),
    ok.

%% --- put, get and delete objects
test_001(undefined,_) ->
    ok;
test_001(State,_) ->
    ACL = undefined,
    Bucket = "bucket001",
    Key = "Key001",
    Value = "Value001",
    ValueBin = list_to_binary(Value),
    ok = ?MUT:delete_bucket(State, Bucket),
    ok = ?MUT:put_bucket(State, Bucket, ACL),
    ok = ?MUT:put_object(State, Bucket, Key, Value,ACL),
    {ok,ValueBin} =
	?MUT:get_object(State, Bucket, Key),
    {ok,_R} = ?MUT:get_bucket(State, Bucket),
    ok = ?MUT:delete_object(State, Bucket, Key),
    ok = ?MUT:delete_bucket(State, Bucket),
    ok.

%% --- get service xml
test_002(undefined,_) ->
    ok;
test_002(State,_) ->
    ACL = undefined,
    Bucket = "bucket002",
    ok = ?MUT:delete_bucket(State, Bucket),
    ok = ?MUT:put_bucket(State, Bucket, ACL),
    {ok,_XML} = ?MUT:get_service_xml(State),
    ok = ?MUT:delete_bucket(State, Bucket),
    ok.

%% --- get service
test_003(undefined,_) ->
    ok;
test_003(State,_) ->
    ACL = undefined,
    Bucket = "bucket002",
    ok = ?MUT:delete_bucket(State, Bucket),
    ok = ?MUT:put_bucket(State, Bucket, ACL),
    {ok,{_Owner,_Buckets}} = ?MUT:get_service(State),
    ok = ?MUT:delete_bucket(State, Bucket),
    ok.

test_zzz(_,_) ->
    ok.

%% ---- internal ---
add_user(Name) ->
    %% gdss_s3_proto's extention for provisioning
    Host = "localhost",
    Port = 23580,
    Header = [{"Host",Host},{"connection","close"},
	      {"x-amz-name",Name}],
    URL = "http://"++Host++":"++integer_to_list(Port)
	++"/",
    Req = {URL, Header, "text/plain", ""},
    {ok,{_,HDR,_}} = httpc:request(put, Req, [], []),
    {?FIELD_KEYID,KeyId}=lists:keyfind(?FIELD_KEYID,1,HDR),
    {?FIELD_KEY,Key}=lists:keyfind(?FIELD_KEY,1,HDR),
					  
    {ok, KeyId, Key}.
