-module(github_handler).
-compile(export_all).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

init(_Transport, Req, []) -> {ok, Req, undefined}.

handle(Req, State) ->
    {Params,NewReq} = cowboy_req:path(Req),
    Path = lists:reverse(string:tokens(binary_to_list(Params),"/")),
    [Repo,User|Rest] = Path,
    Allowed = lists:member(User,["5HT","doxtop","voxoz","synrc","spawnproc"]),
    ResponseReq = case Allowed of
        true ->
            case global:whereis_name("builder") of
                undefined -> spawn(fun() -> global:register_name("builder",self()), builder() end);
                Pid -> global:send("builder",{build,Repo,User}) end,
            HTML = wf:to_binary(["<h1>202 Project Started to Build</h1><a href=\"/index?release=",User,"-",Repo,"\">",User,"-",Repo,"</a>"]),
            {ok, Req3} = cowboy_req:reply(202, [], HTML, NewReq),
            Req3;
        false ->
            NA = wf:to_binary(["<h1>404 User not allowed</h1>"]),
            {ok, Req4} = cowboy_req:reply(404, [], NA, NewReq),
            Req4 end,
    {ok, ResponseReq, State}.

terminate(_Reason, _Req, _State) -> ok.

builder() ->
    receive 
        {build,Repo,User} -> build(Repo,User)
    end, builder().

cmd({User,Repo,Docroot,Buildlogs,LogFolder},No,List) ->
    Message = os:cmd(["cd ",Docroot," && ",List]),
    FileName = binary_to_list(base64:encode(lists:flatten([integer_to_list(No)," ",List]))),
    File = lists:flatten([Buildlogs,"/",LogFolder,"/",FileName]),
    error_logger:info_msg("Command: ~p",[List]),
    error_logger:info_msg("Output: ~p",[Message]),
    file:write_file(File,Message).

build(Repo,User) ->
    Docroot = "repos/" ++ Repo,
    Buildlogs = "buildlogs/"++ User ++ "-" ++ Repo,
    error_logger:info_msg("Hook worker called ~p",[Docroot]),
    {{Y,M,D},{H,Min,S}} = calendar:now_to_datetime(now()),
    LogFolder = io_lib:format("~p-~p-~p ~p:~p:~p",[Y,M,D,H,Min,S]),
    os:cmd(["mkdir -p \"",Docroot,"\""]),
    os:cmd(["mkdir -p \"",Buildlogs,"/",LogFolder,"\""]),
    Ctx = {User,Repo,Docroot,Buildlogs,LogFolder},
    case os:cmd(["ls ",Docroot]) of
        [] -> os:cmd(["git clone git@github.com:",User,"/",Repo,".git ",Docroot]);
        _ -> ok end,
    Script = ["git pull","rebar get-deps","rebar compile","./stop.sh","./release.sh","./styles.sh","./javascript.sh","./start.sh"],
    [ cmd(Ctx,No,lists:nth(No,Script)) || No <- lists:seq(1,length(Script)) ].
