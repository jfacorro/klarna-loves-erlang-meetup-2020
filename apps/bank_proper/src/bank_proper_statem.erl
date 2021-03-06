-module(bank_proper_statem).

-behaviour(proper_statem).

-include("bank_proper.hrl").
-include_lib("proper/include/proper_common.hrl").
-include_lib("stdlib/include/assert.hrl").

-compile(export_all).
-compile(nowarn_export_all).

-include("bank_proper_statem.hrl").

%%==============================================================================
%% The statem's property
%%==============================================================================

prop_main() ->
  setup(),
  ?FORALL( Cmds
         , proper_statem:commands(?MODULE)
         , begin
             cleanup(),
             { History
             , State
             , Result
             } = proper_statem:run_commands(?MODULE, Cmds),
             ?WHENFAIL(
                io:format("History: ~p\nState: ~p\nResult: ~p\nCmds: ~p\n",
                          [ History
                          , State
                          , Result
                          , proper_statem:command_names(Cmds)
                          ]),
                proper:aggregate( proper_statem:command_names(Cmds)
                                , Result =:= ok
                                )
               )
           end
         ).

%%==============================================================================
%% Setup
%%==============================================================================

setup() -> ok.

%%==============================================================================
%% Cleanup
%%==============================================================================

cleanup() -> ok.

%%==============================================================================
%% Initial State
%%==============================================================================

-type account_holder_id() :: binary().
-type account_id()        :: binary().
-type transfer_id()       :: binary().
-type balance()           :: integer().
-type status()            :: created | settled | canceled.
-type transfer()          :: #{ source => account_id()
                              , dest   => account_id()
                              , amount => balance()
                              , status => status()
                              }.

-type state() :: #{ account_holders => #{account_holder_id() => [account_id()]}
                  , accounts        => #{account_id()        => balance()}
                  , transfers       => #{transfer_id()       => transfer()}
                  }.

-spec initial_state() -> state().
initial_state() ->
  #{ account_holders => #{}
   , accounts        => #{}
   , transfers       => #{}
   }.

%%==============================================================================
%% Commands Weights
%%==============================================================================

weight(CmdName) ->
  case CmdName of
    create_account_holder      -> 1;
    get_account                -> 3;
    create_account             -> 1;
    get_transfer               -> 1;
    create_transfer            -> 5;
    update_transfer            -> 1;
    _                          -> 0
  end.

%%==============================================================================
%% create_account
%%==============================================================================

create_account_pre(_S = #{account_holders := AccountHolders}) ->
  maps:size(AccountHolders) > 0.

create_account(BankProperAccountRequest) ->
  bank_proper_api:create_account(BankProperAccountRequest).

create_account_args(_S) ->
  [bank_proper_account_request:bank_proper_account_request()].

%%==============================================================================
%% create_account_holder
%%==============================================================================

create_account_holder(BankProperAccountHolderRequest) ->
  bank_proper_api:create_account_holder(BankProperAccountHolderRequest).

create_account_holder_args(_S) ->
  [bank_proper_account_holder_request:bank_proper_account_holder_request()].

%%==============================================================================
%% create_transfer
%%==============================================================================

create_transfer(BankProperTransferRequest) ->
  bank_proper_api:create_transfer(BankProperTransferRequest).

create_transfer_args(_S) ->
  [bank_proper_transfer_request:bank_proper_transfer_request()].

%%==============================================================================
%% get_account
%%==============================================================================

get_account(Id) ->
  bank_proper_api:get_account(Id).

get_account_args(_S) ->
  [binary()].

%%==============================================================================
%% get_account_holder
%%==============================================================================

get_account_holder(Id) ->
  bank_proper_api:get_account_holder(Id).

get_account_holder_args(_S) ->
  [binary()].

%%==============================================================================
%% get_transfer
%%==============================================================================

get_transfer(Id) ->
  bank_proper_api:get_transfer(Id).

get_transfer_args(_S) ->
  [binary()].

%%==============================================================================
%% healthcheck
%%==============================================================================

healthcheck() ->
  bank_proper_api:healthcheck().

healthcheck_args(_S) ->
  [].

%%==============================================================================
%% ping
%%==============================================================================

ping() ->
  bank_proper_api:ping().

ping_args(_S) ->
  [].

%%==============================================================================
%% update_transfer
%%==============================================================================

update_transfer(Id, BankProperTransferUpdateRequest) ->
  bank_proper_api:update_transfer(Id, BankProperTransferUpdateRequest).

update_transfer_args(_S) ->
  [binary(), bank_proper_transfer_update_request:bank_proper_transfer_update_request()].
