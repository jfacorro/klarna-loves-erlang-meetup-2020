-module(bank_proper_transfer_response).

-include("bank_proper.hrl").

-export([bank_proper_transfer_response/0]).

-export([bank_proper_transfer_response/1]).

-export_type([bank_proper_transfer_response/0]).

-type bank_proper_transfer_response() ::
  [ {'id', binary() }
  | {'source_account_id', binary() }
  | {'destination_account_id', binary() }
  | {'amount', integer() }
  | {'status', bank_proper_transfer_status:bank_proper_transfer_status() }
  ].


bank_proper_transfer_response() ->
    bank_proper_transfer_response([]).

bank_proper_transfer_response(Fields) ->
  Default = [ {'id', binary() }
            , {'source_account_id', binary() }
            , {'destination_account_id', binary() }
            , {'amount', integer() }
            , {'status', bank_proper_transfer_status:bank_proper_transfer_status() }
            ],
  lists:ukeymerge(1, lists:sort(Fields), lists:sort(Default)).

