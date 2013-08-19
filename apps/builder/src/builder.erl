-module(builder).
-compile(export_all).

builder() ->
    receive 
        {build,Repo,User} ->
            build(Repo,User),
            exit(dead); % XXX
        _ ->
            ignore
    end,
    builder().

%% @todo convert to OTP
run_build(Repo, User) ->
    case global:whereis_name(builder) of
        undefined ->
            spawn(fun() ->
                        yes = global:register_name(builder, self()),
                        self() ! {build,Repo,User},
                        builder:builder()
                end);
        Pid when is_pid(Pid) ->
            Pid ! {build,Repo,User}
    end.

build(Repo,User) ->
    Docroot = "repos/" ++ Repo,
    filelib:ensure_dir(Docroot),
    file:make_dir(Docroot),

    Logs = ["buildlogs", "/", User, "-", Repo],
    LogFile = integer_to_list(time_t(now())),
    LogPath = lists:flatten(Logs ++ ["/", LogFile]),
    filelib:ensure_dir(LogPath),
    lager:info("logpath: ~p", [LogPath]),

    sh:run("git clone git://github.com/" ++ User ++ "/" ++ Repo ++ ".git .", LogPath, Docroot),

    Script = case file:read_file_info([Docroot, "/Makefile"]) of
        {ok, _} -> [
                "git pull",
                "git clean -dxf",
                "make"
            ];
        {error, _} -> [
                "git pull",
                "git clean -dxf",
                "rebar get-deps",
                "rebar compile",
                "./stop.sh",
                "./nitrogen_static.sh",
                "./release.sh",
                "./release_sync.sh",
                "./styles.sh",
                "./javascript.sh",
                "./start.sh"
            ]
    end,

    [ sh:run(Cmd, LogPath, Docroot) || Cmd <- Script ].

time_t({Mega, Secs, _Micro}) ->
    Mega*1000*1000 + Secs.
