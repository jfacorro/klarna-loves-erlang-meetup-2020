# bank

An oversimplified bank application to illustrate using property-based
testing with HTTP APIs.

## Build

    $ rebar3 compile

## OpenAPI specification

The API specification is defined in the `openapi.yaml` file.

It is also [published in SwaggerHub][swaggerhub] for a nicer and more user-friendly
view of the API.

## OpenAPI generator

Install the OpenAPI generator by running:

```
curl https://raw.githubusercontent.com/OpenAPITools/openapi-generator/master/bin/utils/openapi-generator-cli.sh > /usr/local/bin/openapitools/openapi-generator-cli
chmod u+x /usr/local/bin/openapi-generator-cli
```

## Generating the HTTP server

Once the OpenAPI generate CLI is available in your path run the
following command to generate the boilerplate code for your HTTP
server.

```bash
openapi-generator-cli generate -g erlang-server -i openapi.yaml -o apps/bank --additional-properties packageName=bank
```

Edit the `bank.app.src` file so that it indicates the `bank_app`
module in the `mod` entry: `{mod, {bank_app, []}`.

Then change the `bank_app:start/2` function as follows:

```erlang
start(_StartType, _StartArgs) ->
  Opts = #{transport_opts => [{ip,{127,0,0,1}},{port,8080}]},
  bank_server:start(bank_http_server, Opts),
  bank_sup:start_link().
```

## Generating the property-based tests

Run the following command to generate the property-based testing
application:

```
openapi-generator-cli generate -g erlang-proper -i openapi.yaml -o apps/bank_proper --additional-properties packageName=bank_proper
```

Since the HTTP server is started in port `8080` we will need to change
the value for the `host` environment variable in `bank_proper.app.src`
to `{host, "http://localhost:8080"}`.

## Running the poper tests

First start the `bank` application's HTTP server with `rebar3 shell`
from the top level directory.

Then start the proper tests by running `rebar3 proper` in the
`apps/bank_proper` directory.

[swaggerhub]: https://app.swaggerhub.com/apis-docs/jfacorro/bank-api/v1
