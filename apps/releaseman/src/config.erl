-module(config).
-compile(export_all).

multiple(Keys) -> [value(Key, "") || Key <- Keys].
value(Key) -> value(Key, "").
value(Key, Default) -> case application:get_env(releaseman,Key) of
                              undefined -> Default;
                              {ok,V} -> V end.
