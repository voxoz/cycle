-module(public_index).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kernel/include/file.hrl").

main() -> [ #dtl{file = "releaseman", app=releaseman, bindings=[{title,title()},{body,body()},{navcollapse, current_status()}]} ].
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
current_status() ->
    Current = try ets:lookup(builder_stat, current) of
        [{current, {Release, Build}}] ->
              #link{class= <<"nav-link">>,
                url="/index/" ++ Release ++ "/" ++ Build, body=[
                    #span{class= <<"glyphicon glyphicon-play">>},
                    "current active build"
                ]}
    catch
        _:_ -> #link{body="no current builds", class= <<"nav-link">>}
    end,
    #ul{class= <<"nav navbar-nav navbar-right">>, body=[ #li{body=Current} ]}.  

builds(Release) ->
    [ #h2{ body = ["builds for ", element(2, release_link(Release))] },
        n2o_bootstrap:link_list_group([
                {"/index/"++Release++"/"++B,
                    "build at " ++ B,
                    revision_url(builder:build_info(Release, B))}
                || B <- builder:all_builds(Release) ])
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
            n2o_bootstrap:link_list_group([{"/index/" ++ R, release_title(R)} || R <- Builds])
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

parse_uri(Url) ->
    http_uri:parse(wf:to_list(Url), [{scheme_defaults, [{git,9418},{https,443},{http,80}]}]).

revision_url(BuildInfo) ->
    Url = proplists:get_value(remote_url, BuildInfo),
    Rev = proplists:get_value(rev, BuildInfo),
    {ok, {_Scheme, _UserInfo, Host, _Port, Path, _Query}} = parse_uri(Url),
    case Host of
        "github.com" ->
            P1 = re:replace(Path, ".git$", "", [{return, binary}]),
            [#link{body= <<"commit ", Rev/binary>>, url= <<"https://github.com", P1/binary, "/commit/", Rev/binary>>}];
        _ -> [<<"commit ", Rev/binary>>]
    end.

release_link(Release) ->
    Url = builder:identify_repo(Release),
    {ok, {_Scheme, _UserInfo, Host, _Port, Path, _Query}} = parse_uri(Url),
    P1 = re:replace(Path, ".git$", "", [{return, binary}]),
    case Host of
        "github.com" ->
            B = <<"github.com", P1/binary>>,
            {B, #link{body= B, url= <<"https://github.com", P1/binary>>}};
        _ ->
            B = <<(list_to_binary(Host))/binary, P1/binary>>,
            {B, [B]}
    end.

release_title(Release) -> element(1, release_link(Release)).
