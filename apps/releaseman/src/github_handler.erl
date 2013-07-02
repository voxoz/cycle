-module(github_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

init(_Transport, Req, []) -> {ok, Req, undefined}.

handle(Req, State) ->
    {ok, Post, Req2} = cowboy_req:body_qs(Req),
    spawn(fun() -> run(Post) end),
    HTML = <<"<h1>202 Project Started to Build</h1>">>,
    {ok, Req3} = cowboy_req:reply(202, [], HTML, Req2),
    {ok, Req3, State}.

terminate(_Reason, _Req, _State) -> ok.

run(Post) ->
    print_with_datetime("~nHook worker called~n"),
    {ok, [Docroot, User, Repo]} = cfgsrv:get_multiple(["server.docroot", "github.username", "github.repository"]),
    [{Bin, true}] = Post,
    _Decoded = mochijson2:decode(Bin), %% Maybe I will do something nice with it... Later
    os:cmd("mkdir -p " ++ Docroot),
    case os:cmd("ls " ++ Docroot) of
        [] -> os:cmd("git clone git@github.com:" ++ User ++ "/" ++ Repo ++ ".git " ++ Docroot);
        _ -> ok end,
    os:cmd("cd " ++ Docroot ++ " && git pull"),
    print_with_datetime("New revision pulled~n").

print_with_datetime(Str) -> error_logger:info_msg("[~p.~p.~p ~p:~p:~p.~p] " ++ Str ++ "~n", datetime()).

datetime() ->
    [{{Y, M, D}, {H, I, S}}, {_ ,_ ,MT}] = [calendar:now_to_local_time(now()), now()],
    [D, M, Y, H, I, S, MT].
