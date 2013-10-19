-module(releaseman_sup).
-behaviour(supervisor).
-include_lib("releaseman/include/releases.hrl").
-export([start_link/0]).
-export([init/1]).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->

    Dispatch = cowboy_router:compile(
        [{'_', [
            {"/static/[...]", cowboy_static, [{directory, {priv_dir, releaseman, [<<"static">>]}},
                                                {mimetypes, {fun mimetypes:path_to_mimes/2, default}}]},
            {"/github/hook/[...]", cycle_handler, []},
            {'_', n2o_cowboy, []}
        ]}
    ]),

    os:cmd("install -d buildlogs"),
    ets:new(releases, [named_table,{keypos,#release.id},public]),
    spawn(fun() -> wf:reg(builder), cycle_handler:loop([]) end),

    {ok, _} = cowboy:start_http(http, 10, [{port, 8989}],[{env, [{dispatch, Dispatch}]}]),

    {ok, {{one_for_one, 5, 10}, []}}.
