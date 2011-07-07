-module(s3_sup).
-export([start_link/0,init/1]).
-behaviour(supervisor).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Id = s3,
    StartFun = {s3, start_link, []},
    CallBackMod = [s3],

    S3 = {Id,StartFun,permanent,5000,worker,CallBackMod},
    {ok,{{one_for_all, 1, 10}, [S3]}}.

