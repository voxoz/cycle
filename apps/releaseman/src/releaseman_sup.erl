-module(releaseman_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->

    {ok, [Port, Url, Nba]} = cfgsrv:get_multiple(["http_server.port", "http_server.url", "http_server.nba"]),
    Dispatch = cowboy_router:compile([
        {'_', [
            {Url, hook_handler, []},
            {'_', default_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_http(http, Nba, [{port, Port}],[{env, [{dispatch, Dispatch}]}]),


    Strategy = {one_for_one, 5, 10},
    Children = [ ],
    {ok, {Strategy, Children}}.
