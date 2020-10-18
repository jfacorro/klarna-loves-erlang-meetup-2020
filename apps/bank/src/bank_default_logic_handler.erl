-module(bank_default_logic_handler).

-behaviour(bank_logic_handler).

-export([handle_request/3]).


-spec handle_request(
    OperationID :: bank_api:operation_id(),
    Req :: cowboy_req:req(),
    Context :: #{}
) ->
    {Status :: cowboy:http_status(), Headers :: cowboy:http_headers(), Body :: jsx:json_term()}.
handle_request('GetAccount', _Req, _Context) ->
    Body = #{ id => <<"ID">>
            , account_holder_id => <<"ID">>
            , balance => 100
            },
    {200, #{}, Body};
handle_request('CreateAccount', _Req, _Context) ->
    Body = #{ id => <<"ID">>
            , account_holder_id => <<"ID">>
            , balance => 100
            },
    {201, #{}, Body};
handle_request('GetAccountHolder', _Req, _Context) ->
    Body = #{ id => <<"ID">>
            , first_name => <<"First">>
            , last_name => <<"Last">>
            },
    {200, #{}, Body};
handle_request('CreateAccountHolder', _Req, _Context) ->
    Body = #{ id => <<"ID">>
            , first_name => <<"First">>
            , last_name => <<"Last">>
            },
    {201, #{}, Body};
handle_request('GetTransfer', _Req, _Context) ->
    Body = #{ id => <<"ID">>
            , source_account_id => <<"ID">>
            , destination_account_id => <<"ID">>
            , amount => 100
            , status => <<"CREATED">>
            },
    {200, #{}, Body};
handle_request('CreateTransfer', _Req, _Context) ->
    Body = #{ id => <<"ID">>
            , source_account_id => <<"ID">>
            , destination_account_id => <<"ID">>
            , amount => 100
            , status => <<"CREATED">>
            },
    {201, #{}, Body};
handle_request('UpdateTransfer', _Req, _Context) ->
    Body = #{ id => <<"ID">>
            , status => <<"CREATED">>
            },
    {201, #{}, Body};
handle_request('Ping', _Req, _Context) ->
    {200, #{}, #{}};
handle_request('Healthcheck', _Req, _Context) ->
    {200, #{}, #{healthy => [], unhealthy => []}};
handle_request(OperationID, Req, Context) ->
    error_logger:error_msg(
        "Got not implemented request to process: ~p~n",
        [{OperationID, Req, Context}]
    ),
    {501, #{}, #{}}.
