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

all_tests_test_() ->
    all_tests_(fun test_setup/0,
               fun test_teardown/1).

all_tests_(Setup,Teardown) ->
    {setup,
     Setup,
     Teardown,
     [
      ?_test(test_000()),
      ?_test(test_001()),
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
    %% assuming s3 server is running on port 23580
    Id = "12345",
    AuthKey=undefined,
    State =
	?MUT:make_state("localhost",23580,Id,AuthKey),
    Bucket = "Bucket000",
    ok = ?MUT:delete_bucket(State, Bucket, undefined),
    ok = ?MUT:put_bucket(State, Bucket, undefined),
    ok = ?MUT:delete_bucket(State, Bucket, undefined),
    ok.

test_001() ->
    application:start(inets),
    %% assuming s3 server is running on port 23580
    Id = "12345",
    AuthKey=undefined,
    State =
	?MUT:make_state("localhost",23580,Id,AuthKey),
    Bucket = "Bucket001",
    Key = "Key001",
    Value = "Value001",
    ValueBin = list_to_binary(Value),
    ok = ?MUT:delete_bucket(State, Bucket, undefined),
    ok = ?MUT:put_bucket(State, Bucket, undefined),
    ok = ?MUT:put_object(State, Bucket, Key, Value,undefined),
    {ok,ValueBin} =
	?MUT:get_object(State, Bucket, Key, undefined),
    ok = ?MUT:delete_object(State, Bucket, Key, undefined),
    ok = ?MUT:delete_bucket(State, Bucket, undefined),
    ok.

test_zzz() ->
    ok.
