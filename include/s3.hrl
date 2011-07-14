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

