-module(bank_proper_account_response).

-include("bank_proper.hrl").

-export([bank_proper_account_response/0]).

-export([bank_proper_account_response/1]).

-export_type([bank_proper_account_response/0]).

-type bank_proper_account_response() ::
  [ {'id', binary() }
  | {'account_holder_id', binary() }
  | {'balance', integer() }
  ].


bank_proper_account_response() ->
    bank_proper_account_response([]).

bank_proper_account_response(Fields) ->
  Default = [ {'id', binary() }
            , {'account_holder_id', binary() }
            , {'balance', integer() }
            ],
  lists:ukeymerge(1, lists:sort(Fields), lists:sort(Default)).

