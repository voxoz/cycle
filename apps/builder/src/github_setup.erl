-module(github_setup).
-compile(export_all).

setup() ->
    ok = add_deploy_key(),
    ok = create_webhook().

generate_ssh_key(Email) ->
    [] = os:cmd("mkdir -p ~/.ssh"),
    [] = os:cmd("ssh-keygen -t rsa -N \"\" -f ~/.ssh/releaseman -C \"" ++ Email ++ "\" -q"),
    [] = os:cmd("echo \"\nHost github.com\n\tIdentityFile ~/.ssh/releaseman\n\" >> ~/.ssh/config").

get_ssh_key(Email) ->
    Key = case os:cmd("if [ -f ~/.ssh/releaseman.pub ]; then cat ~/.ssh/releaseman.pub; else exit 0; fi") of
        [] -> generate_ssh_key(Email),
              os:cmd("cat ~/.ssh/releaseman.pub");
        Res -> Res end,
    [$\n | Out] = lists:reverse(Key),
    lists:reverse(Out).

add_deploy_key() ->
    [User, Email, Password, Repo] = config:multiple([username,mail,password,repository]),
    Key = get_ssh_key(Email),
    Url = "https://api.github.com/repos/" ++ User ++ "/" ++ Repo ++ "/keys",
    H = [{"Authorization", "Basic " ++ base64:encode_to_string(User ++ ":" ++ Password)},
    {"Content-Type", "text/json"}],
    {ok, {{"HTTP/1.1", 200, "OK"}, _Headers, Body}} = httpc:request(get, {Url, H}, [], []),
    [Raw_key, _] = binary:split(list_to_binary(Key), list_to_binary(" " ++ Email)),
    case present_in_response(binary_to_list(Raw_key), Body) of
        true -> ok;
        false -> httpc:request(post, {Url, H, "application/x-www-form-urlencoded",
                    "{\"title\": \"githubizer\", \"key\": \"" ++ Key ++ "\"}"}, [], []),
                 error_logger:info_msg("~n~nDeploy key created~n"), ok end.

present_in_response(_Key, "[]") -> false;
present_in_response(Str, Response) ->
    case string:str(Response, Str) of
        0 -> false;
        _ -> true end.

create_webhook() ->
    [User, Password, Repo, Hook_path, Domain, Port] =
        config:multiple([username,password,repository,url,domain,port]),

    Server_url = case [hd(lists:reverse(Domain))] of
        "/" -> lists:reverse(tl(lists:reverse(Domain))) ++ ":" ++ integer_to_list(Port);
        _ -> Domain ++ ":" ++ integer_to_list(Port) end,
    Hook_url = Server_url ++ Hook_path,
    ApiUrl = "https://api.github.com/repos/" ++ User ++ "/" ++ Repo ++ "/hooks",
    H = [{"Authorization","Basic " ++ base64:encode_to_string(User ++ ":" ++ Password)},
        {"Content-Type", "text/json"},{"User-Agent","VOXOZ"}],
    {ok, {{"HTTP/1.1", 200, "OK"}, _Headers, Content}} = httpc:request(get, {ApiUrl, H}, [], []),
    case present_in_response(Hook_url, Content) of
        true -> ok;
        false -> Body = "{\"name\":\"web\",\"active\":true,\"config\":{\"url\":\"" ++ Hook_url ++ "\",\"content_type\":\"json\"}}",
                httpc:request(post, {ApiUrl, H, "application/x-www-form-urlencoded", Body}, [], []), ok end.
