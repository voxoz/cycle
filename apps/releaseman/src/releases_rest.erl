-module(releases_rest).
-compile(export_all).
-include("releases.hrl").

% PUBLIC REST INTERFACE FOR GLOBAL SERVER

-define(RELS, [#release{id="5HT-skyline",name="5HT-skyline",user="5HT",repo="skyline"}] ).

init() -> ets:new(releases, [named_table,{keypos,#release.id},public]), ets:insert(releases, ?RELS).
get([]) -> ets:foldl(fun(C,Acc) -> [C|Acc] end,[],releases);
get(Id) -> ets:lookup(releases,Id).
delete(Id) -> ets:delete(releases,Id).
put(R=#release{}) -> ets:insert(releases,R).
exists(Id) -> ets:member(releases,Id).
to_html(R=#release{}) -> [<<"<tr><td>">>,coalesce(R#release.id),<<"</td">>,
                                          <<"<td>">>,coalesce(R#release.user),<<"</td><td>">>,
                                          <<"<td>">>,coalesce(R#release.repo),<<"</td><td>">>,
                                                     coalesce(R#release.name),<<"</td></tr>">>].

coalesce(Name) -> case Name of undefined -> <<>>; A -> list_to_binary(A) end.
