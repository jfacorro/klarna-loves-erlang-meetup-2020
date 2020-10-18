%%%-------------------------------------------------------------------
%% @doc bank public API
%% @end
%%%-------------------------------------------------------------------

-module(bank_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Opts = #{ip => {127,0,0,1}, port => 8080, net_opts => []},
    bank_server:start(bank_http_server, Opts),
    bank_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
