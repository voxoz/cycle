-module(sh).
-compile([export_all]).

run(Command, Log, Cwd) when is_list(Command) ->
    Self = self(),
    spawn_link(fun() ->
                {ok, File} = file:open(Log, [append, raw]),
                file:write(File, [">>>> ", Command, "\n"]),

                Port = erlang:open_port({spawn_executable, "/bin/sh"},
                    [stream, stderr_to_stdout, binary, exit_status,
                        {args, ["-c", Command]}, {cd, Cwd}]),

                error_logger:info_msg("~p: ~p", [Port, erlang:port_info(Port)]),
                handle_loop(Self, File)
        end),
    wait().

wait() ->
    receive done -> ok end.

handle_loop(Parent, File) ->
    receive
        {_Port, {data, Data}} ->
            file:write(File, Data),
            handle_loop(Parent, File);
        {_Port, {exit_status, Status}} ->
            file:write(File, [">>>> exit status: ", integer_to_list(Status), "\n"]),
            file:close(File),
            Parent ! done;
        _M ->
            % discard
            handle_loop(Parent, File)
    end.

% t() -> dbg:tracer(), dbg:p(self(), m), dbg:p(new, m), run("false", "/tmp/1", "/tmp").
