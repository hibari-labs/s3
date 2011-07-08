%%%----------------------------------------------------------------------
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
%%%----------------------------------------------------------------------

-module(s3_client_tests).
-include("s3.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(MUT, s3_client). % Module Under Test (a.k.a. DUT)
-define(ATM, ?MODULE). % Automatic Test Module (a.k.a. ATE)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Test Cases
%%

start_1_000_test() ->
    %% application:start(inets),
    %% %% assuming gdss_s3_proto si running on port 23580
    %% State = #state{host = "localhost",
    %% 		   port = 23580,
    %% 		   id = "12345",
    %% 		   auth_key = undefined},
    %% Bucket = "Bucket5",
    %% ok = ?MUT:add_user(State),
    %% ok = ?MUT:delete_bucket(State, Bucket, undefined),
    %% ok = ?MUT:put_bucket(State, Bucket, undefined),
    %% %% ok = ?MUT:get_bucket(State, Bucket),
    %% ok = ?MUT:delete_bucket(State, Bucket, undefined),
    ok.
