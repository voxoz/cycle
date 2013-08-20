-module(builder).
-compile(export_all).

builder() ->
    receive 
        {build, Args} ->
            apply(?MODULE, build, Args),
            exit(dead); % XXX
        _ ->
            ignore
    end,
    builder().

%% @todo convert to OTP
run_build(Opts) ->
    Args = buildargs(Opts),
    case global:whereis_name(builder) of
        undefined ->
            spawn(fun() ->
                        yes = global:register_name(builder, self()),
                        self() ! {build, Args},
                        builder:builder()
                end);
        Pid when is_pid(Pid) ->
            Pid ! {build, Args}
    end.

buildargs(Opts) ->
    error_logger:info_msg("opts: ~p", [Opts]),
    GH = proplists:get_value(<<"github">>, Opts),
    Ref = proplists:get_value(<<"ref">>, Opts, <<"HEAD">>),
    [["git://github.com/", binary_to_list(GH), ".git"], binary_to_list(Ref)].

write_term(Dir, Name, Term) ->
    write_term(filename:join(filename:absname(Dir), Name), Term).
write_term(Path, Term) ->
    filelib:ensure_dir(Path),
    ok = file:write_file(Path, io_lib:format("~p.\n", [Term])).

repo_path(P) -> base64:encode_to_string(P).
identify_repo(P) -> base64:decode_to_string(P).

gather_build_info(Cwd, Target) ->
    Terms = [{Name, run_oneliner(Command, Cwd)} || {Name, Command} <- [
                {remote_url, "git config remote.origin.url"},
                {rev, "git log --format='%H' -1"}
            ]],
    write_term(Target, Terms).

build_log(Release, Build) ->
    N = ["buildlogs/",Release,"/",Build ++ ".log"],
    {ok, Bin} = file:read_file(N),
    Bin.

build_info(Release, Build) ->
    P = filename:join(["buildlogs", Release, Build ++ ".info"]),
    error_logger:info_msg("build_info: ~p", [P]),
    {ok, [T]} = file:consult(P),
    T.

all_builds(Release) ->
    Builds0 = filelib:wildcard("buildlogs/" ++ Release ++ "/*.log"),
    Builds = [begin [_, Release, Build] = filename:split(B), hd(string:tokens(Build, ".")) end || B <- Builds0],
    Builds.

all_releases() ->
    case file:list_dir("buildlogs") of
        {error, E} ->
            error_logger:error_msg("buildlogs: ~p", [E]),
            E;
        {ok, Builds} -> Builds
    end.

build(CloneUrl, Ref) ->
    P = lists:flatten(CloneUrl),
    Repo = repo_path(P),

    Docroot = filename:join(["repos", Repo]),
    filelib:ensure_dir(Docroot),
    file:make_dir(Docroot),

    Logs = filename:join(["buildlogs", Repo]),
    LogFile = integer_to_list(time_t(now())),
    LogPath = filename:join([Logs, LogFile ++ ".log"]),
    filelib:ensure_dir(LogPath),
    error_logger:info_msg("logpath: ~p", [LogPath]),

    sh:run(["git", "clone", "--no-checkout", P, "."], LogPath, Docroot),
    gather_build_info(Docroot, filename:join([Logs, LogFile ++ ".info"])),

    Script = case file:read_file_info([Docroot, "/Makefile"]) of
        {ok, _} -> [
                "git fetch",
                "git clean -dxf",
                ["git", "checkout", Ref],
                "make"
            ];
        {error, _} -> [
                "git fetch",
                "git clean -dxf",
                ["git", "checkout", Ref],
                "rebar get-deps",
                "rebar compile",
                "./stop.sh",
                "./nitrogen_static.sh",
                "./release.sh",
                "./release_sync.sh",
                "./styles.sh",
                "./javascript.sh",
                "./start.sh"
            ]
    end,

    [ sh:run(Cmd, LogPath, Docroot) || Cmd <- Script ].

time_t({Mega, Secs, _Micro}) ->
    Mega*1000*1000 + Secs.

sha1(List) ->
    lists:flatten(io_lib:format("~40.16.0b", [begin <<MM:160>> = crypto:hash(sha, List), MM end])).

run_oneliner(Command, Cwd) ->
    {ok, _, T} = sh:run(Command, binary, Cwd),
    iolist_to_binary(binary:split(T, <<"\n">>, [trim])).
