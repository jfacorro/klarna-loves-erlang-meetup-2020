-module(prop_bank_proper).

-export([prop_test/0]).

prop_test() ->
  {ok, _} = application:ensure_all_started(bank_proper),
  bank_proper_statem:prop_main().
