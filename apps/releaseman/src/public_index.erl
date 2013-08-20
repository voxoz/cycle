-module(public_index).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kernel/include/file.hrl").

main() -> [ #dtl{file = "releaseman", app=releaseman, bindings=[{title,title()},{body,body()}]} ].
title() -> [ <<"vxzbuild">> ].
body() ->
    case lists:reverse(pathlist()) of
        [Build, Release, <<"index">>] ->
            log(binary_to_list(Release),binary_to_list(Build));
        [Release, <<"index">>] ->
            builds(binary_to_list(Release));
        [<<"index">>] ->
            releases();
        [] ->
            releases();
        _ ->
            '404'
    end.

builds(Release) ->
    [ #h2{ body = "builds for " ++ builder:identify_repo(Release) },
        [ #p{ body = [
                    #link { body = B, url= "/index/"++Release++"/"++B },
                    #pre { body = builder:build_info(Release, B) } ]
            } || B <- builder:all_builds(Release) ]
    ].

log(Release,Build) ->
    [ #h4{body=[builder:identify_repo(Release), " build ", Build]},
      #pre{body=builder:build_info(Release, Build)},
      #pre{body=builder:build_log(Release, Build)}
    ].

releases() ->
    New = #p{body=["create new:", #pre{body=[
                    "curl -X POST ", addr(), "/build/proger/erlkit", "\n",
                    "curl -X POST -d 'github=5HT/n2o&ref=action-redirect-fix' ", addr(), "/build"
                ]}]},

    BuildList = case builder:all_releases() of
        [] ->
            [n2o_bootstrap:alert(warning, "no releases")];
        Builds -> 
            n2o_bootstrap:link_list_group([{"/index/" ++ R, builder:identify_repo(R)} || R <- Builds])
    end,

    [ #h2{ body = "releases"}, BuildList, New ].

event(_Event) ->
    %error_logger:error_msg("EVENT: ~p", [Event]),
    ok.

%numeric_compare(S1, S2) ->
%    {I1, _} = string:to_integer(S1),
%    {I2, _} = string:to_integer(S2),
%    I1 < I2.

pathlist() ->
    [B || B <- binary:split(wf:path(), <<"/">>, [global]), B /= <<>>].

addr() ->
    io_lib:format("~s:~B", [config:value(hostname), config:value(port)]).
