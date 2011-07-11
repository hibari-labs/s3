-module(s3_utils).
-export([make_auth/5]).

make_auth(Op, KeyID, Key, Uri, H0) ->
    L0 = [NV || {Name,_Value} = NV <- H0,
		string:substr(Name, 1, 6) =:= "x-amz-"],
    AmzHeaders = orddict:to_list(orddict:from_list(L0)),
    Resource =
        case string:tokens(Uri, "?") of
            [Path, _Qs] ->
                Path;
            [Path] ->
                Path
        end,
    
    Method = string:to_upper(atom_to_list(Op)),
    H0low = lists:map(fun({A,B}) ->
				{string:to_lower(A), B}
			end, H0),
    make_auth(KeyID, Key,
	      Method,
	      %% todo: ignore cases
              proplists:get_value("content-md5", H0low, ""),
              proplists:get_value("content-type", H0low, ""),
              proplists:get_value("date", H0low, ""),
              AmzHeaders,
              Resource, H0).

make_auth(KeyID, KeyData, Verb, ContentMD5, ContentType,
	  Date, AmzHeaders, Resource, H0) ->
    StringToSign =
        [Verb, "\n",
         ContentMD5, "\n",
         ContentType, "\n",
         Date, "\n",
         AmzHeaders,
         Resource],

    Signature = base64:encode_to_string(
		  crypto:sha_mac(KeyData,StringToSign)),
    [{"authorization","AWS"++" "++KeyID++":"++Signature}|H0].
    
