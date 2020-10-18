-module(bank_proper_account_holder_response).

-include("bank_proper.hrl").

-export([bank_proper_account_holder_response/0]).

-export([bank_proper_account_holder_response/1]).

-export_type([bank_proper_account_holder_response/0]).

-type bank_proper_account_holder_response() ::
  [ {'id', binary() }
  | {'first_name', binary() }
  | {'last_name', binary() }
  ].


bank_proper_account_holder_response() ->
    bank_proper_account_holder_response([]).

bank_proper_account_holder_response(Fields) ->
  Default = [ {'id', binary() }
            , {'first_name', binary() }
            , {'last_name', binary() }
            ],
  lists:ukeymerge(1, lists:sort(Fields), lists:sort(Default)).

