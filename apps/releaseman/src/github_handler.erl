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
    Allowed = lists:member(User, config:value(allowed_users)),
    ResponseReq = case Allowed of
        true ->
            case global:whereis_name("builder") of
                undefined -> spawn(fun() -> global:register_name("builder",self()), builder:builder() end);
                Pid -> global:send("builder",{build,Repo,User}) end,

            Output = wf:to_binary(["build started: /index?release=",User,"-",Repo,""]),
            {ok, Req3} = cowboy_req:reply(202, [], Output, NewReq),
            Req3;
        false ->
            NA = wf:to_binary(["user not allowed"]),
            {ok, Req4} = cowboy_req:reply(403, [], NA, NewReq),
            Req4 end,
    {ok, ResponseReq, State}.

terminate(_Reason, _Req, _State) -> ok.

create_release(User,Repo) ->
    ets:insert(releases,#release{user=User,repo=Repo,id=User++"/"++Repo,name=User++"/"++Repo}).

list_releases() -> ets:foldl(fun(C,Acc) -> [C|Acc] end,[],releases).

