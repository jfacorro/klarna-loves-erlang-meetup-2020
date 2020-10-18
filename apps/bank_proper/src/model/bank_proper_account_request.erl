-module(bank_proper_account_request).

-include("bank_proper.hrl").

-export([bank_proper_account_request/0]).

-export([bank_proper_account_request/1]).

-export_type([bank_proper_account_request/0]).

-type bank_proper_account_request() ::
  [ {'account_holder_id', binary() }
  | {'balance', integer() }
  ].


bank_proper_account_request() ->
    bank_proper_account_request([]).

bank_proper_account_request(Fields) ->
  Default = [ {'account_holder_id', binary() }
            , {'balance', integer() }
            ],
  lists:ukeymerge(1, lists:sort(Fields), lists:sort(Default)).

