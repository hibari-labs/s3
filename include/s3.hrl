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
-define(S3_PATH_STYLE, s3_path_style).
-define(S3_VIRTUAL_HOSTED_STYLE, s3_virtual_hosted_style).

-record(state, {host     :: string(),
		port     :: integer(),
		id       :: string(),
		auth_key :: string(),
		style    :: ?S3_PATH_STYLE |
                            ?S3_VIRTUAL_HOSTED_STYLE}).


%% --- XML ---
-record(owner, {id           :: string(),
		display_name :: string()}).

-record(bucket, {name          :: string(),
		 creation_date :: string()}).

-record(content,
	{key :: string(),
	 last_modified :: string(),
	 etag :: string(),
	 size :: integer(),
	 storage_class :: string(),
	 owner :: #owner{}}).

-record(list_bucket,
	{name :: string(),
	 prefix :: string(),
	 marker :: string(),
	 max_keys :: integer(),
	 is_truncated :: boolean(),
	 contents :: [#content{}]}).

