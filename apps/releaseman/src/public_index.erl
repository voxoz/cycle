-module(public_index).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kernel/include/file.hrl").
-include("releases.hrl").

main() -> [ #dtl{file = "releaseman", app=releaseman, bindings=[{title,title()},{body,body()}]} ].
title() -> [ <<"RELEASE MANAGER">> ].
body() ->
    Ctx = wf_context:context(),
    lager:error("~p", [wf:path(Ctx#context.req)]),
    case {wf:qs(<<"release">>),wf:qs(<<"build">>)} of
      {undefined,undefined} -> releases();
      {Release,undefined}   -> builds(binary_to_list(Release));
      {Release,Build}       -> log(binary_to_list(Release),binary_to_list(Build))
    end.

builds(Release) ->
    {ok, Builds} = file:list_dir(["buildlogs/",Release]),

    [ #h2{ body = "Builds for " ++ Release },
      [ #p{ body = #link { body = B, url= "/index?release="++Release++"&build="++B }} || B <- Builds ]
    ].

log(Release,Build) ->
    N = ["buildlogs/",Release,"/",Build],
    {ok,Bin} = file:read_file(N),

    [ #h2{body=N}, #pre{body=Bin} ].

releases() ->
    BuildList = case file:list_dir("buildlogs") of
        {error, E} ->
            error_logger:error_msg("buildlogs: ~p", E),
            #p{ body = "None yet." };
        {ok, Builds} ->
            [ #p{ body = #link { body = R, url= "/index?release="++R }} || R <- Builds ]
    end,

%    create_release() ++
    [
        #h1{ body = "Continuous Integration"},
        #h2{ body = "Releases" },
        BuildList
    ].

create_release() ->
    [ #h2{ body = "Create Release" },
    #textbox{ },
    #dropdown { options = [#option{ label= "Simple",value= "simple"},#option{ label= "Rich Web",value="richweb"}]},
    #button{ body = "Create", postback=create_release }, #br{},
    #checkbox { body="new", postback=stage}, #panel {id=stage},
    #br{} ].

event(init) -> ok;

event(stage) -> 
    wf:update(stage,
    #dropdown { options = [#option{ label= "Simple",value= "simple"},#option{ label= "Rich Web",value="richweb"}]}
    ).

numeric_compare(S1, S2) ->
    {I1, _} = string:to_integer(S1),
    {I2, _} = string:to_integer(S2),
    I1 < I2.
