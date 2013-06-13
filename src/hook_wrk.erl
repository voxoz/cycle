-module(hook_wrk).
-export([run/1]).

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
