-module(bank_router).

-export([get_paths/1]).

-type operations() :: #{
    Method :: binary() => bank_api:operation_id()
}.

-type init_opts()  :: {
    Operations :: operations(),
    LogicHandler :: atom(),
    ValidatorState :: jesse_state:state()
}.

-export_type([init_opts/0]).

-spec get_paths(LogicHandler :: atom()) ->  [{'_',[{
    Path :: string(),
    Handler :: atom(),
    InitOpts :: init_opts()
}]}].

get_paths(LogicHandler) ->
    ValidatorState = prepare_validator(),
    PreparedPaths = maps:fold(
        fun(Path, #{operations := Operations, handler := Handler}, Acc) ->
            [{Path, Handler, Operations} | Acc]
        end,
        [],
        group_paths()
    ),
    [
        {'_',
            [{P, H, {O, LogicHandler, ValidatorState}} || {P, H, O} <- PreparedPaths]
        }
    ].

group_paths() ->
    maps:fold(
        fun(OperationID, #{path := Path, method := Method, handler := Handler}, Acc) ->
            case maps:find(Path, Acc) of
                {ok, PathInfo0 = #{operations := Operations0}} ->
                    Operations = Operations0#{Method => OperationID},
                    PathInfo = PathInfo0#{operations => Operations},
                    Acc#{Path => PathInfo};
                error ->
                    Operations = #{Method => OperationID},
                    PathInfo = #{handler => Handler, operations => Operations},
                    Acc#{Path => PathInfo}
            end
        end,
        #{},
        get_operations()
    ).

get_operations() ->
    #{ 
        'CreateAccount' => #{
            path => "/v1/accounts",
            method => <<"POST">>,
            handler => 'bank_default_handler'
        },
        'CreateAccountHolder' => #{
            path => "/v1/account-holders",
            method => <<"POST">>,
            handler => 'bank_default_handler'
        },
        'CreateTransfer' => #{
            path => "/v1/transfers",
            method => <<"POST">>,
            handler => 'bank_default_handler'
        },
        'GetAccount' => #{
            path => "/v1/accounts/:id",
            method => <<"GET">>,
            handler => 'bank_default_handler'
        },
        'GetAccountHolder' => #{
            path => "/v1/account-holders/:id",
            method => <<"GET">>,
            handler => 'bank_default_handler'
        },
        'GetTransfer' => #{
            path => "/v1/transfers/:id",
            method => <<"GET">>,
            handler => 'bank_default_handler'
        },
        'Healthcheck' => #{
            path => "/v1/healtcheck",
            method => <<"GET">>,
            handler => 'bank_default_handler'
        },
        'Ping' => #{
            path => "/v1/ping",
            method => <<"GET">>,
            handler => 'bank_default_handler'
        },
        'UpdateTransfer' => #{
            path => "/v1/transfers/:id",
            method => <<"PATCH">>,
            handler => 'bank_default_handler'
        }
    }.

prepare_validator() ->
    R = jsx:decode(element(2, file:read_file(get_openapi_path()))),
    jesse_state:new(R, [{default_schema_ver, <<"http://json-schema.org/draft-04/schema#">>}]).


get_openapi_path() ->
    {ok, AppName} = application:get_application(?MODULE),
    filename:join(bank_utils:priv_dir(AppName), "openapi.json").


