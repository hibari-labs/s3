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
                    #list_bucket{contents=[]},
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
            [lists:foldl(fun bucket0/2,#bucket{},Cntnt)|Acc];
        _ ->
            Acc
    end;
buckets0(_,Acc) ->
    Acc.

bucket0(#xmlElement{name=Name,content=[Text]}, Bucket) ->
    case Name of
        'Name' ->
            Bucket#bucket{name=Text#xmlText.value};
        'CreationDate' ->
            Bucket#bucket{creation_date=Text#xmlText.value};
        _ ->
            Bucket
    end;
bucket0(_,Acc) ->
    Acc.


list_bucket0(#xmlElement{name=Name,content=Cntnt}, Acc) ->
    Value = text_value(Cntnt),
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
            C0 = Acc#list_bucket.contents,
            Acc#list_bucket
                {contents =
                     [lists:foldl(fun contents0/2,
                                  #content{}, Cntnt)|
                      C0]};
        _ ->
            Acc
    end;
list_bucket0(_,Acc) ->
    Acc.

contents0(#xmlElement{name=Name,content=Cntnt}, Acc) ->
    Value = text_value(Cntnt),
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
              owner=lists:foldl(fun owner0/2, #owner{}, Cntnt)};
        _ ->
            Acc
    end;
contents0(_,Acc) ->
    Acc.

text_value(Cntnt) ->
    case Cntnt of
        [X] when is_record(X, xmlText) ->
            X#xmlText.value;
        [] ->
            "";
        _ ->
            {error,not_text}
    end.
