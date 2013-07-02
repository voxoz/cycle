-module(releaseman_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->

    Dispatch = cowboy_router:compile(
        [{'_', [
            {"/static/[...]", cowboy_static, [{directory, {priv_dir, releaseman, [<<"static">>]}},
                                                {mimetypes, {fun mimetypes:path_to_mimes/2, default}}]},
            {"/rest/:bucket",            n2o_rest, []}, %% for releases REST interface
            {"/rest/:bucket/:key",       n2o_rest, []},
            {"/rest/:bucket/:key/[...]", n2o_rest, []},
            {"/github/hook/[...]", github_handler, []},
            {'_', n2o_cowboy, []}
        ]}
    ]),

    releases_rest:init(),

    {ok, _} = cowboy:start_http(http, 10, [{port, config:value(port)}],[{env, [{dispatch, Dispatch}]}]),

    {ok, {{one_for_one, 5, 10}, []}}.
