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
    all_tests_(fun test_setup/0,
               fun test_teardown/1).

all_tests_(Setup,Teardown) ->
    {setup,
     Setup,
     Teardown,
     [
      %% ?_test(test_000()),
      %% ?_test(test_001()),
      ?_test(test_zzz())
     ]
    }.

test_setup() ->
    ok.

test_teardown(_) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Test Cases
%%

test_000() ->
    application:start(inets),
    %% provisioning
    {ok, Id, AuthKey} = add_user("test_user000"),

    %% assuming s3 server is running on port 23580
    State =
	?MUT:make_state("localhost",23580,Id,AuthKey),
    ACL = undefined,
    Bucket = "Bucket000",
    ok = ?MUT:delete_bucket(State, Bucket),
    ok = ?MUT:put_bucket(State, Bucket, ACL),
    ok = ?MUT:delete_bucket(State, Bucket),
    ok.

test_001() ->
    application:start(inets),
    %% provisioning
    {ok, Id, AuthKey} = add_user("test_user001"),

    %% assuming s3 server is running on port 23580
    State =
	?MUT:make_state("localhost",23580,Id,AuthKey),
    ACL = undefined,
    Bucket = "Bucket001",
    Key = "Key001",
    Value = "Value001",
    ValueBin = list_to_binary(Value),
    ok = ?MUT:delete_bucket(State, Bucket),
    ok = ?MUT:put_bucket(State, Bucket, ACL),
    ok = ?MUT:put_object(State, Bucket, Key, Value,ACL),
    {ok,ValueBin} =
	?MUT:get_object(State, Bucket, Key),
    ok = ?MUT:delete_object(State, Bucket, Key),
    ok = ?MUT:delete_bucket(State, Bucket),
    ok.

test_zzz() ->
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
