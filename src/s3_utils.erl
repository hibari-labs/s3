-module(s3_utils).
-export([make_auth/5,
	 xml_list_bucket/1,
	 xml_service/1]).

-include("s3.hrl").
-include_lib("xmerl/include/xmerl.hrl").

make_auth(Op, KeyID, Key, Uri, H0) ->
    H0low = lists:map(fun({A,B}) ->
				{string:to_lower(A), B}
			end, H0),
    AmzHeaders = fix_header(H0low),
    Resource = fix_resource(Uri),
    
    Method = string:to_upper(atom_to_list(Op)),
    make_auth(KeyID, Key,
	      Method,
	      %% todo: ignore cases
              proplists:get_value("content-md5", H0low, ""),
              proplists:get_value("content-type", H0low, ""),
              proplists:get_value("date", H0low, ""),
              AmzHeaders,
              Resource, H0).

xml_service(XML) ->
    {Top, []} = xmerl_scan:string(binary_to_list(XML)),
    ReturnL =
	lists:foldl(fun service0/2,
		    {#owner{}, []},
		    Top#xmlElement.content),
    {ok, ReturnL}.

xml_list_bucket(XML) ->
    {Top, []} = xmerl_scan:string(binary_to_list(XML)),
    ReturnL =
	lists:foldl(fun list_bucket0/2,
		    #list_bucket{},
		    Top#xmlElement.content),
    {ok, ReturnL}.


%% --- internal -------------------------------------------
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

fix_header(Header) ->
    %% todo - header fields with the same name into one field
    %%      - unfold long field  
    L0 = [{Name,Value} ||
	     {Name, Value} <- Header,
	     string:substr(Name, 1, 6) =:= "x-amz-"],
    lists:map(fun({N,V}) -> N++":"++V++"\n" end,
	      orddict:to_list(orddict:from_list(L0))).

fix_resource(Uri) ->
    %% todo - prepend bucket for virtual-hosted style
    %%      - subresources like "?versioning"
    case string:tokens(Uri, "?") of
	[Path, _Qs] ->
	    Path;
	[Path] ->
	    Path
    end.
    
service0(#xmlElement{name=Name,content=Cntnt},
	 {Owner0,Buckets0} = Acc) ->
    case Name of
	'Owner' ->
	    {lists:foldl(fun owner0/2, Owner0, Cntnt),
	     Buckets0};
	'Buckets' ->
	    {Owner0,
	     lists:foldl(fun buckets0/2, Buckets0, Cntnt)};
	_ ->
	     Acc
    end;
service0(_,Acc) ->
    Acc.

owner0(#xmlElement{name=Name,content=[Text]}, Owner) ->
    case Name of
	'ID' ->
	    Owner#owner{id=Text#xmlText.value};
	'DisplayName' ->
	    Owner#owner{display_name=Text#xmlText.value};
	_ ->
	     Owner
    end;
owner0(_,Acc) ->
    Acc.

buckets0(#xmlElement{name=Name,content=Cntnt}, Acc) ->
    case Name of
	'Bucket' ->
	    lists:foldl(fun bucket0/2, Acc, Cntnt);
	_ ->
	     Acc
    end;
buckets0(_,Acc) ->
    Acc.

bucket0(#xmlElement{name=Name,content=[Text]}, Buckets) ->
    case Name of
	'Name' ->
	    [#bucket{name=Text#xmlText.value}|Buckets];
	'CreationDate' ->
	    [H|T] = Buckets,
	    [H#bucket{creation_date=Text#xmlText.value}|T];
	_ ->
	     Buckets
    end;
bucket0(_,Acc) ->
    Acc.


list_bucket0(#xmlElement{name=Name,content=Cntnt}, Acc) ->
    Value = case Cntnt of
		[X] when is_record(X, xmlText) ->
		    X#xmlText.value;
		[] ->
		    ""
	    end,
    case Name of
	'Name' ->
	    Acc#list_bucket{name=Value};
	'Prefix' ->
	    Acc#list_bucket{prefix=Value};
	'Marker' ->
	    Acc#list_bucket{marker=Value};
	'MaxKeys' ->
	    Acc#list_bucket{max_keys=list_to_integer(Value)};
	'IsTruncated' ->
	    Acc#list_bucket{is_truncated=list_to_atom(Value)};
	'Contents' ->
	    lists:foldl(fun contents0/2, Acc, Cntnt);
	_ ->
	     Acc
    end;
list_bucket0(_,Acc) ->
    Acc.

contents0(#xmlElement{name=Name,content=Cntnt}, Acc) ->
    Value = case Cntnt of
		[X] when is_record(X, xmlText) ->
		    X#xmlText.value;
		[] ->
		    ""
	    end,
    case Name of
	'Key' ->
	    Acc#content{key=Value};
	'LastModified' ->
	    Acc#content{last_modified=Value};
	'ETag' ->
	    Acc#content{etag=Value};
	'Size' ->
	    Acc#content{size=list_to_integer(Value)};
	'StorageClass' ->
	    Acc#content{storage_class=Value};
	'Owner' ->
	    Acc#content{
	      owner=lists:foldl(fun owner0/2, Acc, Cntnt)};
	_ ->
	     Acc
    end;
contents0(_,Acc) ->
    Acc.
