-module(default_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

init(_Transport, Req, []) -> {ok, Req, undefined}.

handle(Req, State) ->
    HTML = <<"<h1>404 Page Not Found</h1>">>,
    {ok, Req2} = cowboy_req:reply(404, [], HTML, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) -> ok.