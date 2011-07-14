-define(S3_PATH_STYLE, s3_path_style).
-define(S3_VIRTUAL_HOSTED_STYLE, s3_virtual_hosted_style).

-record(state, {host     :: string(),
		port     :: integer(),
		id       :: string(),
		auth_key :: string(),
		style    :: ?S3_PATH_STYLE |
                            ?S3_VIRTUAL_HOSTED_STYLE}).


-record(owner, {id           :: string(),
		display_name :: string()}).

-record(bucket, {name          :: string(),
		 creation_date :: string()}).
