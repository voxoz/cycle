-module(cycle_handler).
-compile(export_all).
-include("releases.hrl").
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

init(_Transport, Req, []) -> {ok, Req, undefined}.
terminate(_Reason, _Req, _State) -> ok.

handle(Req, State) ->
    {Params,NewReq} = cowboy_req:path(Req),
    Path = lists:reverse(string:tokens(binary_to_list(Params),"/")),
    [Repo,User|Rest] = Path,
    {Status,HTML} = req(Repo,User),
    {ok, ResponseReq} = cowboy_req:reply(Status, [], HTML, NewReq),
    {ok, ResponseReq, State}.

req(Repo,User) ->
    Name = [User,"-",Repo],
    wf:send(builder,{build_req,Repo,User,self()}),
    receive
        queued -> {202,wf:to_binary(["<h1>202 Build Started</h1><a href=\"/index?release=",Name,"\">",Name,"</a>"])};
        denied -> {404,wf:to_binary(["<h1>404 User not allowed</h1>"])};
        locked -> {202,wf:to_binary(["<h1>202 Queue Locked</h1>Try again later"])};
        Unknown -> wf:info("Unknown ACK: ~p",[Unknown]) end.

loop(State) -> ?MODULE:loop(
    receive 
        {build_done} -> wf:info("Build Done"), [];
        {build,Repo,User,Pid} -> 
            wf:info("Build ~p",[{Repo,User,Pid}]),
            case lists:member(User,["5HT","doxtop","voxoz","synrc","spawnproc"]) of
                false -> Pid ! denied, [];
                true  -> spawn(fun() -> build(Repo,User), wf:send(builder,{build_done}) end),
                         Pid ! queued, State end;
        {build_req,Repo,User,Pid} -> 
            wf:info("Build Request ~p",[{Repo,User,Pid}]),
            case State of
                [] -> wf:send(builder,{build,Repo,User,Pid}), [{Repo,User}];
                _  -> Pid ! locked, State end;
        Command -> wf:info("Unknown Builder Command ~p ",[Command]) end).

cmd({User,Repo,Docroot,Buildlogs,LogFolder},No,List) ->
    wf:info("Command: ~p in ~p",[List,Docroot]),
    Message = os:cmd(["cd ",Docroot," && ",List]),
    [ErrorCode|_] = lists:reverse(string:tokens(Message,"\n")),
    FileName = binary_to_list(base64:encode(lists:flatten([wf:f("~2..0B",[No])," ",List," ",ErrorCode]))),
    File = lists:flatten([Buildlogs,"/",LogFolder,"/",FileName]),
    file:write_file(File,Message).

build(Repo,User) ->
    Docroot = "repos/" ++ User ++ "-" ++ Repo,
    wf:info("Hook worker called ~p",[Docroot]),
    Buildlogs = "buildlogs/"++ User ++ "-" ++ Repo,
    {{Y,M,D},{H,Min,S}} = calendar:universal_time_to_local_time(calendar:now_to_datetime(now())),
    LogFolder = io_lib:format("~p-~p-~p ~p:~p:~p",[Y,M,D,H,Min,S]),
    os:cmd(["install -d ",Docroot]),
    os:cmd(["install -d \"",Buildlogs,"/",LogFolder,"\""]),
    Ctx = {User,Repo,Docroot,Buildlogs,LogFolder},
    case os:cmd(["ls ",Docroot]) of
        [] -> os:cmd(["git clone git://github.com/",User,"/",Repo,".git ",Docroot]);
        _ -> ok end,
    Script = [  "./stop.sh","./update.sh","./compile.sh",
                "./nitrogen_static.sh","./release.sh","./release_sync.sh",
                "./styles.sh","./javascript.sh","./start.sh"],
    [ cmd(Ctx,No,lists:nth(No,Script)) || No <- lists:seq(1,length(Script)) ].

load_releases(Repo) -> Rels = string:tokens(os:cmd("ls -1 buildlogs/"++Repo),"\n"), [ create(R,Repo) || R <-Rels].
load() -> Repos = string:tokens(os:cmd("ls -1 buildlogs"),"\n"), [ load_releases(R) || R <- Repos ].
list() -> lists:keysort(3,ets:foldl(fun(C,Acc) -> [C|Acc] end,[],releases)).
create(Release,Project) ->
    [User,Repo] = string:tokens(Project,"-"),
    ets:insert(releases,#release{user=User,repo=Repo,id=User++"/"++Repo++" "++Release,name=Release}).
