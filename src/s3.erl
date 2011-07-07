-module(s3).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1
         , handle_call/3
         , handle_cast/2
         , handle_info/2
         , terminate/2
         , code_change/3
        ]).

-record(state, {none_yet}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init(_) ->
    {ok, #state{}}.

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
