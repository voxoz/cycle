-module(sh).
-export([run/1, run/2, run/3, run/4]).

run(C) ->
    run(C, binary, ".").

run(C, Log) ->
    run(C, Log, ".").

run([C|Args], Log, Cwd) when is_list(C) ->
    run(os:find_executable(C), Args, Log, Cwd);
run(Command, Log, Cwd) when is_list(Command) ->
    run("/bin/sh", ["-c", Command], Log, Cwd).

run(Command, Args, binary, Cwd) ->
    Self = self(),
    spawn_link(fun() ->
                Port = erlang:open_port({spawn_executable, Command},
                    [stream, stderr_to_stdout, binary, exit_status,
                        {args, Args}, {cd, Cwd}]),

                error_logger:info_msg("~p: ~p", [Port, erlang:port_info(Port)]),
                handle_loop(Self, binary, [])
        end),
    wait();
run(Command, Args, Log, Cwd) ->
    Self = self(),
    spawn_link(fun() ->
                {ok, File} = file:open(Log, [append, raw]),
                file:write(File, [">>> ", ts(), " ", Command, " ", [[A, " "] || A <- Args], "\n"]),

                Port = erlang:open_port({spawn_executable, Command},
                    [stream, stderr_to_stdout, binary, exit_status,
                        {args, Args}, {cd, Cwd}]),

                error_logger:info_msg("~p: ~p", [Port, erlang:port_info(Port)]),
                handle_loop(Self, File)
        end),
    wait().

wait() ->
    receive {done, Status, Data} -> {ok, Status, Data} end.

handle_loop(Parent, binary, Acc) ->
    receive
        {_Port, {data, Data}} ->
            handle_loop(Parent, binary, [Acc, Data]);
        {_Port, {exit_status, Status}} ->
            Parent ! {done, Status, iolist_to_binary(Acc)};
        _M -> % discard
            handle_loop(Parent, binary, Acc)
    end.

handle_loop(Parent, File) ->
    receive
        {_Port, {data, Data}} ->
            file:write(File, Data),
            handle_loop(Parent, File);
        {_Port, {exit_status, Status}} ->
            file:write(File, [">>> ", ts(), " exit status: ", integer_to_list(Status), "\n"]),
            file:close(File),
            Parent ! {done, Status, undefined};
        _M -> % discard
            handle_loop(Parent, File)
    end.

% t() -> dbg:tracer(), dbg:p(self(), m), dbg:p(new, m), run("false", "/tmp/1", "/tmp").

ts() ->
    Ts = {{_Y,_M,_D},{_H,_Min,_S}} = calendar:now_to_datetime(now()),
    io_lib:format("~p", [Ts]).
