-module(cycle_index).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("releaseman/include/releases.hrl").

main() -> [ #dtl{file = "releaseman", app=releaseman,bindings=[{title,title()},{body,body()}]} ].
title() -> [ <<"RELEASE MANAGER">> ].
body() ->
    case {wf:qs(<<"release">>),wf:qs(<<"build">>),wf:qs(<<"log">>)} of
      {undefined,undefined,undefined} -> releases();
      {Release,undefined,undefined}   -> builds(wf:to_list(Release));
      {Release,Build,undefined}       -> steps(wf:to_list(Release),wf:to_list(Build));
      {Release,Build,Step}            -> log(wf:to_list(Release),wf:to_list(Build),wf:to_list(Step));
                                    _ -> releases() end.

builds(Release) ->
    wf:info("builds: ~p",[Release]),
    Builds = string:tokens(os:cmd(["ls -1 buildlogs/",Release]),"\n"),
    [ #h2{ body = "Builds for " ++ Release },
      [ #p{ body = #link { body = B, url= "/index?release="++Release++"&build="++B }} || B <- Builds ]
    ].

steps(Release,Build) ->
    wf:info("steps: ~p ~p",[Release,Build]),
    Steps = string:tokens(os:cmd(["ls -1 \"buildlogs/",Release,"/",Build,"\""]),"\n"),
    [ #h2{ body = "Steps for " ++ Build ++ " build of " ++ Release ++ " release" },
      [ #p{ body = #link { body = base64:decode(wf:to_list(S)),
            url= "/index?release="++Release++"&build="++Build++"&log="++http_uri:encode(S) }}
        || S <- lists:sort(Steps) ] ].

log(Release,Build,Step) ->
    wf:info("log: ~p ~p ~p",[Release,Build,Step]),
    {ok,Bin} = file:read_file(["buildlogs/",Release,"/",Build,"/",Step]),
    [<<"<pre>">>,Bin,<<"</pre>">>].

releases() ->
    Builds = string:tokens(os:cmd(["ls -1 buildlogs"]),"\n"),
%    create_release() ++
    [ #h1{ body = "Continuous Integration"}, #h2{ body = "Stages" },
      [ #p{ body = #link { body = R, url= "/index?release="++R }} || R <- Builds ] ,
      #br{},#br{},#br{},
      #span{ body = "&copy; Synrc Research Center" }
    ].

create_release() ->
    [ #h2{ body = "Create Release" },
    #textbox{ },
    #dropdown { options = [#option{ label= "Simple",value= "simple"},#option{ label= "Rich Web",value="richweb"}]},
    #button{ body = "Create", postback=create_release }, #br{},
    #checkbox { body="new", postback=stage}, #panel {id=stage},
    #br{} ].

event(stage) -> 
    wf:update(stage,
    #dropdown { options = [#option{ label= "Simple",value= "simple"},#option{ label= "Rich Web",value="richweb"}]}
    ).
