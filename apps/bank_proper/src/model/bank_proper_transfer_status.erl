-module(bank_proper_transfer_status).

-include("bank_proper.hrl").

-export([bank_proper_transfer_status/0]).

-export_type([bank_proper_transfer_status/0]).

-type bank_proper_transfer_status() ::
  binary().

bank_proper_transfer_status() ->
  elements([<<"CREATED">>, <<"SETTLED">>, <<"CANCELED">>]).

