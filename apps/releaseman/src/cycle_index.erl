-module(cycle_index).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("releaseman/include/releases.hrl").

main() -> [ #dtl{file = "releaseman", app=releaseman,bindings=[{title,title()},{body,body()}]} ].
title() -> [ <<"RELEASE MANAGER">> ].
body() ->
    case {q(<<"release">>),q(<<"build">>),q(<<"log">>)} of
        {[],[],[]} -> releases();
        {Release,[],[]} -> builds(Release);
        {Release,Build,[]} -> steps(Release,Build);
        {Release,Build,Step} -> log(Release,Build,Step);
        _ -> releases() end.

releases() ->
    Builds = string:tokens(os:cmd(["ls -1 buildlogs"]),"\n"),
    [ #h1{ body = "Continuous Integration Cycle"}, 
      #h2{ body = "Five Supreme"},
      [ #p{ body = #link { body = R#release.id,
        url="/index?release="++R#release.user ++ "-" ++ R#release.repo ++ "&build=" ++ R#release.name}}
        || R <- lists:sublist(lists:reverse(cycle_handler:list()),5) ],
      #h2{ body = "Stages" },
      [ #p{ body = #link { body = R, url= "/index?release="++R }} || R <- Builds ] ].

builds(Release) ->
    Builds = string:tokens(os:cmd(["ls -1 buildlogs/",Release]),"\n"),
    [ release(Release), #br{},
      [ #p{ body = #link { body = B, url= "/index?release="++Release++"&build="++B }} || B <- Builds ] ].

steps(Release,Build) ->
    Steps = string:tokens(os:cmd(["ls -1 \"buildlogs/",Release,"/",Build,"\""]),"\n"),
    [ release(Release), build(Build), #br{},
      [ #p{ body = #link { body = base64:decode(wf:to_list(S)), class=[status(S)],
            url= "/index?release="++Release++"&build="++Build++"&log="++http_uri:encode(S) }}
        || S <- lists:sort(Steps) ] ].

log(Release,Build,Step) ->
    {ok,Bin} = file:read_file(["buildlogs/",Release,"/",Build,"/",Step]),
    [ release(Release), build(Build), step(Step), #br{}, <<"<pre>">>,Bin,<<"</pre>">>].

q(S) -> wf:to_list(wf:qs(S)).
release(Release) -> #h1{body= Release ++ " release"}.
build(Build) -> #h2{ body = "Steps for " ++ Build ++ " build"  }.
step(Step) -> #h3{body = "Step " ++ wf:to_list(base64:decode(Step))}.
status(Step) -> Tokens = lists:reverse(string:tokens(wf:to_list(base64:decode(Step))," ")),
    [Code|_] = Tokens, wf:info("Code: ~p",[Code]), case Code of "0" -> "green"; _ -> "red" end.
