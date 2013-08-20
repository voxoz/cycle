-module(build_handler).
-compile(export_all).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {PathBin, Req1} = cowboy_req:path(Req),
    {ok, Params, Req2} = cowboy_req:body_qs(Req1),
    
    % @todo github json

    case Params of
        [] -> % curl -X POST localhost:8989/build/proger/sync
            Path = lists:reverse(string:tokens(binary_to_list(PathBin),"/")),
            [Repo,User|_Px] = Path,
            Allowed = lists:member(User, config:value(allowed_users)),

            ResponseReq = case Allowed of
                true ->
                    builder:run_build([{<<"github">>, list_to_binary(User ++ "/" ++ Repo)}]),

                    Output = <<"build started\n">>,
                    {ok, Req3} = cowboy_req:reply(202, [], Output, Req2),
                    Req3;
                false ->
                    NA = <<"user not allowed">>,
                    {ok, Req4} = cowboy_req:reply(403, [], NA, Req2),
                    Req4
            end,
            {ok, ResponseReq, State};

        _ -> % curl -X POST -d 'github=proger/sync&ref=HEAD' localhost:8989/build
            builder:run_build(Params),
            Output = <<"{ok, params}\n">>,
            {ok, Req3} = cowboy_req:reply(202, [], Output, Req2),
            {ok, Req3, State}
    end.

terminate(_Reason, _Req, _State) ->
    ok.
