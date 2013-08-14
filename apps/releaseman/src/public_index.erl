-module(public_index).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kernel/include/file.hrl").
-include("releases.hrl").

main() -> [ #dtl{file = "releaseman", app=releaseman,bindings=[{title,title()},{body,body()}]} ].
title() -> [ <<"RELEASE MANAGER">> ].
body() ->
    case {wf:qs(<<"release">>),wf:qs(<<"build">>),wf:qs(<<"log">>)} of
      {undefined,undefined,undefined} -> releases();
      {Release,undefined,undefined}   -> builds(binary_to_list(Release));
      {Release,Build,undefined}       -> steps(binary_to_list(Release),binary_to_list(Build));
      {Release,Build,Step}            -> log(binary_to_list(Release),binary_to_list(Build),binary_to_list(Step)) end.

builds(Release) ->
    error_logger:info_msg("builds: ~p",[Release]),
    {ok, Builds} = file:list_dir(["buildlogs/",Release]),

    [ #h2{ body = "Builds for " ++ Release },
      [ #p{ body = #link { body = B, url= "/index?release="++Release++"&build="++B }} || B <- Builds ]
    ].

steps(Release,Build) ->
    {ok, Steps} = file:list_dir(["buildlogs/",Release,"/",Build]),

    [ #h2{ body = "Steps of " ++ Build ++ " build of " ++ Release ++ " release" },
        #list{ body = [
                #li{ body=[
                        #link {
                            body = S,
                            url = "/index?release="++Release++"&build="++Build++"&log="++StepFile },
                        #pre{
                            body = step(Release, Build, StepFile)
                        }
                    ]
                } || {StepFile, S} <- lists:sort(
                    fun({_, T1}, {_, T2}) -> numeric_compare(T1, T2) end,
                    lists:zip(Steps, [base64:decode_to_string(St) || St <- Steps]))
            ]
        }
    ].

step(Release, Build, Step) ->
    {ok,Bin} = file:read_file(["buildlogs/",Release,"/",Build,"/",Step]),
    Bin.

log(Release,Build,Step) ->
    error_logger:info_msg("log: ~p ~p ~p",[Release,Build,Step]),
    [<<"<pre>">>,step(Release, Build, Step),<<"</pre>">>].

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

event(stage) -> 
    wf:update(stage,
    #dropdown { options = [#option{ label= "Simple",value= "simple"},#option{ label= "Rich Web",value="richweb"}]}
    ).

numeric_compare(S1, S2) ->
    {I1, _} = string:to_integer(S1),
    {I2, _} = string:to_integer(S2),
    I1 < I2.
