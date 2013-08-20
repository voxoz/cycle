-module(public_index).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kernel/include/file.hrl").
-include("releases.hrl").

pathlist() ->
    [B || B <- binary:split(wf:path(), <<"/">>, [global]), B /= <<>>].

addr() ->
    io_lib:format("~s:~B", [config:value(hostname), config:value(port)]).

main() -> [ #dtl{file = "releaseman", app=releaseman, bindings=[{title,title()},{body,body()}]} ].
title() -> [ <<"vxzbuild">> ].
body() ->
    io:format("~p~n", [pathlist()]),
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
    Builds0 = filelib:wildcard("buildlogs/" ++ Release ++ "/*.log"),
    Builds = [begin [_, Release, Build] = filename:split(B), hd(string:tokens(Build, ".")) end || B <- Builds0],

    [ #h2{ body = "builds for " ++ builder:identify_repo(Release) },
        [ #p{ body = [
                    #link { body = B, url= "/index/"++Release++"/"++B },
                    #pre { body = builder:build_info(Release, B) }
                ] } || B <- Builds ]
    ].

log(Release,Build) ->
    N = ["buildlogs/",Release,"/",Build ++ ".log"],
    {ok, Bin} = file:read_file(N),

    [ #h2{body=N}, #pre{body=Bin} ].

releases() ->
    BuildList = case file:list_dir("buildlogs") of
        {error, E} ->
            error_logger:error_msg("buildlogs: ~p", [E]),
            [
                #p{ body = ["no releases"] },
                #p{ body = ["create new: ", #pre{body=["curl -X POST ", addr(), "/build/proger/erlkit"]}] }
            ];
        {ok, Builds} ->
            [ #p{ body = #link { body = builder:identify_repo(R), url= "/index/"++R }} || R <- Builds ]
    end,

    [ #h2{ body = "vxzbuild"}, BuildList ].

event(init) ->
    ok.

%numeric_compare(S1, S2) ->
%    {I1, _} = string:to_integer(S1),
%    {I2, _} = string:to_integer(S2),
%    I1 < I2.
