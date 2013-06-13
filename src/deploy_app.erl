-module(deploy_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Sup = githubizer_sup:start_link(),
    {ok, [Port, Url, Nba]} = cfgsrv:get_multiple(["http_server.port", "http_server.url", "http_server.nba"]),
    Dispatch = cowboy_router:compile([
        {'_', [
            {Url, hook_handler, []},
            {'_', default_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_http(http, Nba, [{port, Port}],[{env, [{dispatch, Dispatch}]}]),
    Sup.

stop(_State) ->
    ok.
