-module(bank_proper_transfer_request).

-include("bank_proper.hrl").

-export([bank_proper_transfer_request/0]).

-export([bank_proper_transfer_request/1]).

-export_type([bank_proper_transfer_request/0]).

-type bank_proper_transfer_request() ::
  [ {'source_account_id', binary() }
  | {'destination_account_id', binary() }
  | {'amount', integer() }
  ].


bank_proper_transfer_request() ->
    bank_proper_transfer_request([]).

bank_proper_transfer_request(Fields) ->
  Default = [ {'source_account_id', binary() }
            , {'destination_account_id', binary() }
            , {'amount', integer() }
            ],
  lists:ukeymerge(1, lists:sort(Fields), lists:sort(Default)).

