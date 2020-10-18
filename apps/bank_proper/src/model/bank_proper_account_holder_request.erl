-module(bank_proper_account_holder_request).

-include("bank_proper.hrl").

-export([bank_proper_account_holder_request/0]).

-export([bank_proper_account_holder_request/1]).

-export_type([bank_proper_account_holder_request/0]).

-type bank_proper_account_holder_request() ::
  [ {'first_name', binary() }
  | {'last_name', binary() }
  ].


bank_proper_account_holder_request() ->
    bank_proper_account_holder_request([]).

bank_proper_account_holder_request(Fields) ->
  Default = [ {'first_name', binary() }
            , {'last_name', binary() }
            ],
  lists:ukeymerge(1, lists:sort(Fields), lists:sort(Default)).

