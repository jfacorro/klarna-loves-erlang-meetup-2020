%%%-------------------------------------------------------------------
%% @doc bank public API
%% @end
%%%-------------------------------------------------------------------

-module(bank_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Opts = #{transport_opts => [{ip,{127,0,0,1}},{port,8080}]},
    bank_server:start(bank_http_server, Opts),
    bank_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
