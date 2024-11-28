-module(bank_default_handler).
-moduledoc """
Exposes the following operation IDs:

- `POST` to `/accounts`, OperationId: `createAccount`:
Create an Account.


- `POST` to `/account-holders`, OperationId: `createAccountHolder`:
Create an Account Holder.


- `POST` to `/transfers`, OperationId: `createTransfer`:
Create a Transfer.


- `GET` to `/accounts/:id`, OperationId: `getAccount`:
Get an Account.


- `GET` to `/account-holders/:id`, OperationId: `getAccountHolder`:
Get an Account Holder.


- `GET` to `/transfers/:id`, OperationId: `getTransfer`:
Get a Transfer.


- `GET` to `/healtcheck`, OperationId: `healthcheck`:
Healthcheck.


- `GET` to `/ping`, OperationId: `ping`:
Ping.


- `PATCH` to `/transfers/:id`, OperationId: `updateTransfer`:
Update a Transfer&#39;s status.


""".

-behaviour(cowboy_rest).

-include_lib("kernel/include/logger.hrl").

%% Cowboy REST callbacks
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([delete_resource/2]).
-export([is_authorized/2]).
-export([valid_content_headers/2]).
-export([handle_type_accepted/2, handle_type_provided/2]).

-ignore_xref([handle_type_accepted/2, handle_type_provided/2]).

-export_type([class/0, operation_id/0]).

-type class() :: 'default'.

-type operation_id() ::
    'createAccount' %% Create an Account
    | 'createAccountHolder' %% Create an Account Holder
    | 'createTransfer' %% Create a Transfer
    | 'getAccount' %% Get an Account
    | 'getAccountHolder' %% Get an Account Holder
    | 'getTransfer' %% Get a Transfer
    | 'healthcheck' %% Healthcheck
    | 'ping' %% Ping
    | 'updateTransfer'. %% Update a Transfer&#39;s status


-record(state,
        {operation_id :: operation_id(),
         accept_callback :: bank_logic_handler:accept_callback(),
         provide_callback :: bank_logic_handler:provide_callback(),
         api_key_handler :: bank_logic_handler:api_key_callback(),
         context = #{} :: bank_logic_handler:context()}).

-type state() :: #state{}.

-spec init(cowboy_req:req(), bank_router:init_opts()) ->
    {cowboy_rest, cowboy_req:req(), state()}.
init(Req, {Operations, Module}) ->
    Method = cowboy_req:method(Req),
    OperationID = maps:get(Method, Operations, undefined),
    ?LOG_INFO(#{what => "Attempt to process operation",
                method => Method,
                operation_id => OperationID}),
    State = #state{operation_id = OperationID,
                   accept_callback = fun Module:accept_callback/4,
                   provide_callback = fun Module:provide_callback/4,
                   api_key_handler = fun Module:authorize_api_key/2},
    {cowboy_rest, Req, State}.

-spec allowed_methods(cowboy_req:req(), state()) ->
    {[binary()], cowboy_req:req(), state()}.
allowed_methods(Req, #state{operation_id = 'createAccount'} = State) ->
    {[<<"POST">>], Req, State};
allowed_methods(Req, #state{operation_id = 'createAccountHolder'} = State) ->
    {[<<"POST">>], Req, State};
allowed_methods(Req, #state{operation_id = 'createTransfer'} = State) ->
    {[<<"POST">>], Req, State};
allowed_methods(Req, #state{operation_id = 'getAccount'} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, #state{operation_id = 'getAccountHolder'} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, #state{operation_id = 'getTransfer'} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, #state{operation_id = 'healthcheck'} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, #state{operation_id = 'ping'} = State) ->
    {[<<"GET">>], Req, State};
allowed_methods(Req, #state{operation_id = 'updateTransfer'} = State) ->
    {[<<"PATCH">>], Req, State};
allowed_methods(Req, State) ->
    {[], Req, State}.

-spec is_authorized(cowboy_req:req(), state()) ->
    {true | {false, iodata()}, cowboy_req:req(), state()}.
is_authorized(Req, State) ->
    {true, Req, State}.

-spec content_types_accepted(cowboy_req:req(), state()) ->
    {[{binary(), atom()}], cowboy_req:req(), state()}.
content_types_accepted(Req, #state{operation_id = 'createAccount'} = State) ->
    {[
      {<<"application/json">>, handle_type_accepted}
     ], Req, State};
content_types_accepted(Req, #state{operation_id = 'createAccountHolder'} = State) ->
    {[
      {<<"application/json">>, handle_type_accepted}
     ], Req, State};
content_types_accepted(Req, #state{operation_id = 'createTransfer'} = State) ->
    {[
      {<<"application/json">>, handle_type_accepted}
     ], Req, State};
content_types_accepted(Req, #state{operation_id = 'getAccount'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'getAccountHolder'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'getTransfer'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'healthcheck'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'ping'} = State) ->
    {[], Req, State};
content_types_accepted(Req, #state{operation_id = 'updateTransfer'} = State) ->
    {[
      {<<"application/json">>, handle_type_accepted}
     ], Req, State};
content_types_accepted(Req, State) ->
    {[], Req, State}.

-spec valid_content_headers(cowboy_req:req(), state()) ->
    {boolean(), cowboy_req:req(), state()}.
valid_content_headers(Req, #state{operation_id = 'createAccount'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'createAccountHolder'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'createTransfer'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'getAccount'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'getAccountHolder'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'getTransfer'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'healthcheck'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'ping'} = State) ->
    {true, Req, State};
valid_content_headers(Req, #state{operation_id = 'updateTransfer'} = State) ->
    {true, Req, State};
valid_content_headers(Req, State) ->
    {false, Req, State}.

-spec content_types_provided(cowboy_req:req(), state()) ->
    {[{binary(), atom()}], cowboy_req:req(), state()}.
content_types_provided(Req, #state{operation_id = 'createAccount'} = State) ->
    {[
      {<<"application/json">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'createAccountHolder'} = State) ->
    {[
      {<<"application/json">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'createTransfer'} = State) ->
    {[
      {<<"application/json">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'getAccount'} = State) ->
    {[
      {<<"application/json">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'getAccountHolder'} = State) ->
    {[
      {<<"application/json">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'getTransfer'} = State) ->
    {[
      {<<"application/json">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'healthcheck'} = State) ->
    {[
      {<<"application/json">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, #state{operation_id = 'ping'} = State) ->
    {[], Req, State};
content_types_provided(Req, #state{operation_id = 'updateTransfer'} = State) ->
    {[
      {<<"application/json">>, handle_type_provided}
     ], Req, State};
content_types_provided(Req, State) ->
    {[], Req, State}.

-spec delete_resource(cowboy_req:req(), state()) ->
    {boolean(), cowboy_req:req(), state()}.
delete_resource(Req, State) ->
    {Res, Req1, State1} = handle_type_accepted(Req, State),
    {true =:= Res, Req1, State1}.

-spec handle_type_accepted(cowboy_req:req(), state()) ->
    { bank_logic_handler:accept_callback_return(), cowboy_req:req(), state()}.
handle_type_accepted(Req, #state{operation_id = OperationID,
                                 accept_callback = Handler,
                                 context = Context} = State) ->
    {Res, Req1, Context1} = Handler(default, OperationID, Req, Context),
    {Res, Req1, State#state{context = Context1}}.

-spec handle_type_provided(cowboy_req:req(), state()) ->
    {cowboy_req:resp_body(), cowboy_req:req(), state()}.
handle_type_provided(Req, #state{operation_id = OperationID,
                                 provide_callback = Handler,
                                 context = Context} = State) ->
    {Res, Req1, Context1} = Handler(default, OperationID, Req, Context),
    {Res, Req1, State#state{context = Context1}}.
