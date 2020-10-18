-module(bank_proper_healthcheck_response).

-include("bank_proper.hrl").

-export([bank_proper_healthcheck_response/0]).

-export([bank_proper_healthcheck_response/1]).

-export_type([bank_proper_healthcheck_response/0]).

-type bank_proper_healthcheck_response() ::
  [ {'healthy', list(binary()) }
  | {'unhealthy', list(binary()) }
  ].


bank_proper_healthcheck_response() ->
    bank_proper_healthcheck_response([]).

bank_proper_healthcheck_response(Fields) ->
  Default = [ {'healthy', list(binary()) }
            , {'unhealthy', list(binary()) }
            ],
  lists:ukeymerge(1, lists:sort(Fields), lists:sort(Default)).

