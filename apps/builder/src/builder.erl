-module(builder).
-compile(export_all).

builder() ->
    receive 
        {build,Repo,User} -> build(Repo,User)
    end,
    builder().

cmd({User,Repo,Docroot,Buildlogs,LogFolder},No,List) ->
    Message = os:cmd(["cd ",Docroot," && ",List]),
    FileName = binary_to_list(base64:encode(lists:flatten([integer_to_list(No)," ",List]))),
    File = lists:flatten([Buildlogs,"/",LogFolder,"/",FileName]),
    error_logger:info_msg("Command: ~p",[List]),
    error_logger:info_msg("Output: ~p",[Message]),
    file:write_file(File,Message).

create_dir(Docroot) -> os:cmd("mkdir -p \"" ++ Docroot ++ "\"").

build(Repo,User) ->
    Docroot = "repos/" ++ Repo,
    Buildlogs = "buildlogs/"++ User ++ "-" ++ Repo,
    error_logger:info_msg("Hook worker called ~p",[Docroot]),
    {{Y,M,D},{H,Min,S}} = calendar:universal_time_to_local_time(calendar:now_to_datetime(now())),
    LogFolder = io_lib:format("~p-~p-~p ~p:~p:~p",[Y,M,D,H,Min,S]),
    error_logger:info_msg("Mkdir Docroot ~p",[]),
    os:cmd(["mkdir -p ",Docroot]),
    os:cmd(["mkdir -p \"",Buildlogs,"/",LogFolder,"\""]),
    Ctx = {User,Repo,Docroot,Buildlogs,LogFolder},
    case os:cmd(["ls ",Docroot]) of
        [] -> os:cmd(["git clone git://github.com/",User,"/",Repo,".git ",Docroot]);
        _ -> ok
    end,

    Script = [
        "git pull",
        "rebar delete-deps",
        "rebar get-deps",
        "rebar compile",
        "./stop.sh",
        "./nitrogen_static.sh",
        "./release.sh",
        "./release_sync.sh",
        "./styles.sh",
        "./javascript.sh",
        "./start.sh",
        "rebar ling-build-image"
    ],

    [ cmd(Ctx,No,lists:nth(No,Script)) || No <- lists:seq(1,length(Script)) ].

