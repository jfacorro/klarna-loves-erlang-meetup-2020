-module(bank_proper_api).

-export([ create_account/1
        , create_account_holder/1
        , create_transfer/1
        , get_account/1
        , get_account_holder/1
        , get_transfer/1
        , healthcheck/0
        , ping/0
        , update_transfer/2
        ]).

-define(BASE_URL, "/v1").

%% @doc Create an Account
%% 
-spec create_account(bank_proper_account_request:bank_proper_account_request()) ->
  bank_proper_utils:response().
create_account(BankProperAccountRequest) ->
  Method      = post,
  Host        = application:get_env(bank_proper, host, "http://localhost:8080"),
  Path        = ["/accounts"],
  Body        = BankProperAccountRequest,
  ContentType = hd(["application/json"]),

  bank_proper_utils:request(Method, [Host, ?BASE_URL, Path], jsx:encode(Body), ContentType).

%% @doc Create an Account Holder
%% 
-spec create_account_holder(bank_proper_account_holder_request:bank_proper_account_holder_request()) ->
  bank_proper_utils:response().
create_account_holder(BankProperAccountHolderRequest) ->
  Method      = post,
  Host        = application:get_env(bank_proper, host, "http://localhost:8080"),
  Path        = ["/account-holders"],
  Body        = BankProperAccountHolderRequest,
  ContentType = hd(["application/json"]),

  bank_proper_utils:request(Method, [Host, ?BASE_URL, Path], jsx:encode(Body), ContentType).

%% @doc Create a Transfer
%% 
-spec create_transfer(bank_proper_transfer_request:bank_proper_transfer_request()) ->
  bank_proper_utils:response().
create_transfer(BankProperTransferRequest) ->
  Method      = post,
  Host        = application:get_env(bank_proper, host, "http://localhost:8080"),
  Path        = ["/transfers"],
  Body        = BankProperTransferRequest,
  ContentType = hd(["application/json"]),

  bank_proper_utils:request(Method, [Host, ?BASE_URL, Path], jsx:encode(Body), ContentType).

%% @doc Get an Account
%% 
-spec get_account(binary()) ->
  bank_proper_utils:response().
get_account(Id) ->
  Method      = get,
  Host        = application:get_env(bank_proper, host, "http://localhost:8080"),
  Path        = ["/accounts/", Id, ""],

  bank_proper_utils:request(Method, [Host, ?BASE_URL, Path]).

%% @doc Get an Account Holder
%% 
-spec get_account_holder(binary()) ->
  bank_proper_utils:response().
get_account_holder(Id) ->
  Method      = get,
  Host        = application:get_env(bank_proper, host, "http://localhost:8080"),
  Path        = ["/account-holders/", Id, ""],

  bank_proper_utils:request(Method, [Host, ?BASE_URL, Path]).

%% @doc Get a Transfer
%% 
-spec get_transfer(binary()) ->
  bank_proper_utils:response().
get_transfer(Id) ->
  Method      = get,
  Host        = application:get_env(bank_proper, host, "http://localhost:8080"),
  Path        = ["/transfers/", Id, ""],

  bank_proper_utils:request(Method, [Host, ?BASE_URL, Path]).

%% @doc Healthcheck
%% 
-spec healthcheck() ->
  bank_proper_utils:response().
healthcheck() ->
  Method      = get,
  Host        = application:get_env(bank_proper, host, "http://localhost:8080"),
  Path        = ["/healtcheck"],

  bank_proper_utils:request(Method, [Host, ?BASE_URL, Path]).

%% @doc Ping
%% 
-spec ping() ->
  bank_proper_utils:response().
ping() ->
  Method      = get,
  Host        = application:get_env(bank_proper, host, "http://localhost:8080"),
  Path        = ["/ping"],

  bank_proper_utils:request(Method, [Host, ?BASE_URL, Path]).

%% @doc Update a Transfer's status
%% 
-spec update_transfer(binary(), bank_proper_transfer_update_request:bank_proper_transfer_update_request()) ->
  bank_proper_utils:response().
update_transfer(Id, BankProperTransferUpdateRequest) ->
  Method      = patch,
  Host        = application:get_env(bank_proper, host, "http://localhost:8080"),
  Path        = ["/transfers/", Id, ""],
  Body        = BankProperTransferUpdateRequest,
  ContentType = hd(["application/json"]),

  bank_proper_utils:request(Method, [Host, ?BASE_URL, Path], jsx:encode(Body), ContentType).

