-module(n2o_bootstrap).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").

alert(Type, Body) when is_atom(Type) ->
    #'div'{class= <<"alert alert-", (erlang:atom_to_binary(Type, latin1))/binary>>, body=Body}.

panel(Type, Header, Body, Footer) when is_atom(Type) ->
    #'div'{class= <<"panel panel-primary">>, body=[
        case Header of [_|_] -> #'div'{class= <<"panel-heading">>, body=#h3{class= <<"panel-title">>, body=Header}}; _ -> [] end,
        #'div'{class= <<"panel-body">>, body=Body},
        case Footer of [_|_] -> #'div'{class= <<"panel-footer">>, body=#h3{class= <<"panel-title">>, body=Footer}}; _ -> [] end
    ]}.

list_group(Items) ->
    #ul{class= <<"list-group">>,
        body= [#li{class= <<"list-group-item">>, body=Item} || Item <- Items]}.

link_list_group(Items) ->
    #'div'{ class= <<"list-group">>, body= [ #link {
                class= <<"list-group-item">>,
                body= Body,
                url= Url
            } || {Url, Body} <- Items ]
    }.

