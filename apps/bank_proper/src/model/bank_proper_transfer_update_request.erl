-module(bank_proper_transfer_update_request).

-include("bank_proper.hrl").

-export([bank_proper_transfer_update_request/0]).

-export([bank_proper_transfer_update_request/1]).

-export_type([bank_proper_transfer_update_request/0]).

-type bank_proper_transfer_update_request() ::
  [ {'status', bank_proper_transfer_status:bank_proper_transfer_status() }
  ].


bank_proper_transfer_update_request() ->
    bank_proper_transfer_update_request([]).

bank_proper_transfer_update_request(Fields) ->
  Default = [ {'status', bank_proper_transfer_status:bank_proper_transfer_status() }
            ],
  lists:ukeymerge(1, lists:sort(Fields), lists:sort(Default)).

