-module(public_index).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("releaseman/include/releases.hrl").

main() -> [ #dtl{file = "index", app=releaseman,bindings=[{title,title()},{body,body()}]} ].
title() -> [ <<"RELEASE MANAGER">> ].
body() ->
    case {wf:qs(<<"release">>),wf:qs(<<"build">>),wf:qs(<<"log">>)} of
      {undefined,undefined,undefined} -> releases();
      {Release,undefined,undefined}   -> builds(binary_to_list(Release));
      {Release,Build,undefined}       -> steps(binary_to_list(Release),binary_to_list(Build));
      {Release,Build,Step}            -> log(binary_to_list(Release),binary_to_list(Build),binary_to_list(Step)) end.

builds(Release) ->
    error_logger:info_msg("builds: ~p",[Release]),
    Builds = string:tokens(os:cmd(["ls -1 buildlogs/",Release]),"\n"),
    [ #h2{ body = "Builds for " ++ Release },
      [ #p{ body = #link { body = B, url= "/index?release="++Release++"&build="++B }} || B <- Builds ]
    ].

steps(Release,Build) ->
    error_logger:info_msg("steps: ~p ~p",[Release,Build]),
    Steps = string:tokens(os:cmd(["ls -1 \"buildlogs/",Release,"/",Build,"\""]),"\n"),
    [ #h2{ body = "Steps for " ++ Build ++ " build of " ++ Release ++ " release" },
      [ #p{ body = #link { body = wf:to_list(base64:decode(S)), url= "/index?release="++Release++"&build="++Build++"&log="++S }} || S <- lists:sort(Steps) ]
    ].

log(Release,Build,Step) ->
    error_logger:info_msg("log: ~p ~p ~p",[Release,Build,Step]),
    {ok,Bin} = file:read_file(["buildlogs/",Release,"/",Build,"/",Step]),
    [<<"<pre>">>,Bin,<<"</pre>">>].

releases() ->
    Builds = string:tokens(os:cmd(["ls -1 buildlogs"]),"\n"),
    [ #h1{ body = "Continuos Integration"},
      create_release(),
      #h2{ body = "Stages" },
      [ #p{ body = #link { body = R, url= "/index?release="++R }} || R <- Builds ] ,
      #br{},#br{},#br{},
      #span{ body = "&copy; Synrc Research Center" }
    ].

create_release() ->
    [ #h2{ body = "Create Release" },
    #textbox{ },
    #dropdown { options = [#option{ label="simple",value="simple"},#option{ label="richweb",value="richweb"}]},
    #button{ body = "Create", postback=create_release }, #br{},
    #checkbox { body="new", postback=stage}, #panel {id=stage},
    #br{} ].

event(stage) -> 
    wf:update(stage,
      #dropdown { options = [#option{ label="simple",value="simple"},#option{ label="richweb",value="richweb"}]}
    ).