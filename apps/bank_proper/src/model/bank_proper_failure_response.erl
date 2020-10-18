-module(bank_proper_failure_response).

-include("bank_proper.hrl").

-export([bank_proper_failure_response/0]).

-export([bank_proper_failure_response/1]).

-export_type([bank_proper_failure_response/0]).

-type bank_proper_failure_response() ::
  [ {'error_messages', list(binary()) }
  ].


bank_proper_failure_response() ->
    bank_proper_failure_response([]).

bank_proper_failure_response(Fields) ->
  Default = [ {'error_messages', list(binary()) }
            ],
  lists:ukeymerge(1, lists:sort(Fields), lists:sort(Default)).

