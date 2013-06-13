-module(hook_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

init(_Transport, Req, []) -> {ok, Req, undefined}.

handle(Req, State) ->
    {ok, Post, Req2} = cowboy_req:body_qs(Req),
    spawn(hook_wrk, run, [Post]),
    HTML = <<"<h1>404 Page Not Found</h1>">>,
    {ok, Req3} = cowboy_req:reply(404, [], HTML, Req2),
    {ok, Req3, State}.

terminate(_Reason, _Req, _State) -> ok.
