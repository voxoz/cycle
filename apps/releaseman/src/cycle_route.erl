-module (cycle_route).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-export(?ROUTING_API).

finish(State, Ctx) -> {ok, State, Ctx}.
init(State, Ctx) -> 
    Path = wf:path(Ctx#context.req),
    {ok, State, Ctx#context{path=Path,module=route(Path)}}.

route(<<"/">>) -> cycle_index;
route(<<"/index">>) -> cycle_index;
route(<<"/ws/">>) -> cycle_index;
route(<<"/ws/index">>) -> cycle_index;
route(<<"/favicon.ico">>) -> static_file;
route(_) -> cycle_index.
