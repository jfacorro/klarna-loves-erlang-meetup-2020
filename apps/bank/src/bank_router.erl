-module(bank_router).

-export([get_paths/1]).

-type method() :: binary().
-type operations() :: #{method() => bank_api:operation_id()}.
-type init_opts()  :: {operations(), module()}.

-export_type([init_opts/0]).

-spec get_paths(LogicHandler :: module()) -> cowboy_router:routes().
get_paths(LogicHandler) ->
    PreparedPaths = maps:fold(
                      fun(Path, #{operations := Operations, handler := Handler}, Acc) ->
                              [{Path, Handler, Operations} | Acc]
                      end, [], group_paths()
                     ),
    [{'_', [{P, H, {O, LogicHandler}} || {P, H, O} <- PreparedPaths]}].

group_paths() ->
    maps:fold(
      fun(OperationID, #{servers := Servers, base_path := BasePath, path := Path,
                         method := Method, handler := Handler}, Acc) ->
              FullPaths = build_full_paths(Servers, BasePath, Path),
              merge_paths(FullPaths, OperationID, Method, Handler, Acc)
      end, #{}, get_operations()).

build_full_paths([], BasePath, Path) ->
    [lists:append([BasePath, Path])];
build_full_paths(Servers, _BasePath, Path) ->
    [lists:append([Server, Path]) || Server <- Servers ].

merge_paths(FullPaths, OperationID, Method, Handler, Acc) ->
    lists:foldl(
      fun(Path, Acc0) ->
              case maps:find(Path, Acc0) of
                  {ok, PathInfo0 = #{operations := Operations0}} ->
                      Operations = Operations0#{Method => OperationID},
                      PathInfo = PathInfo0#{operations => Operations},
                      Acc0#{Path => PathInfo};
                  error ->
                      Operations = #{Method => OperationID},
                      PathInfo = #{handler => Handler, operations => Operations},
                      Acc0#{Path => PathInfo}
              end
      end, Acc, FullPaths).

get_operations() ->
    #{ 
       'createAccount' => #{
            servers => [],
            base_path => "/v1",
            path => "/accounts",
            method => <<"POST">>,
            handler => 'bank_default_handler'
        },
       'createAccountHolder' => #{
            servers => [],
            base_path => "/v1",
            path => "/account-holders",
            method => <<"POST">>,
            handler => 'bank_default_handler'
        },
       'createTransfer' => #{
            servers => [],
            base_path => "/v1",
            path => "/transfers",
            method => <<"POST">>,
            handler => 'bank_default_handler'
        },
       'getAccount' => #{
            servers => [],
            base_path => "/v1",
            path => "/accounts/:id",
            method => <<"GET">>,
            handler => 'bank_default_handler'
        },
       'getAccountHolder' => #{
            servers => [],
            base_path => "/v1",
            path => "/account-holders/:id",
            method => <<"GET">>,
            handler => 'bank_default_handler'
        },
       'getTransfer' => #{
            servers => [],
            base_path => "/v1",
            path => "/transfers/:id",
            method => <<"GET">>,
            handler => 'bank_default_handler'
        },
       'healthcheck' => #{
            servers => [],
            base_path => "/v1",
            path => "/healtcheck",
            method => <<"GET">>,
            handler => 'bank_default_handler'
        },
       'ping' => #{
            servers => [],
            base_path => "/v1",
            path => "/ping",
            method => <<"GET">>,
            handler => 'bank_default_handler'
        },
       'updateTransfer' => #{
            servers => [],
            base_path => "/v1",
            path => "/transfers/:id",
            method => <<"PATCH">>,
            handler => 'bank_default_handler'
        }
    }.
