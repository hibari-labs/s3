-module(s3_app).
-behaviour(application).
-export([start/2,stop/1]).

start(_Type, _StartArgs) ->
  s3_sup:start_link().
stop(_State) ->
  ok.

