-module(github_handler).
-compile(export_all).
-include("releases.hrl").
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
                undefined -> spawn(fun() -> global:register_name("builder",self()), builder:builder() end);
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

create_release(User,Repo) ->
    ets:insert(releases,#release{user=User,repo=Repo,id=User++"/"++Repo,name=User++"/"++Repo}).

list_releases() -> ets:foldl(fun(C,Acc) -> [C|Acc] end,[],releases).

