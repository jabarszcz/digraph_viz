-module(digraph_viz).

%% API exports
-export([
    export/1,
    export/2,
    export/4
  ]).

-export_types([
    option/0
  ]).

%%====================================================================
%% API types
%%====================================================================

-type digraph_elem() :: digraph:vertex() | digraph:edge().

-type option() :: {name, string()}
                | {attr_dict, dict:dict(digraph_elem(), {atom(), any()})}.

%%====================================================================
%% API functions
%%====================================================================

-spec export(digraph:digraph()) -> iolist().
export(Digraph) ->
    export(Digraph, []).

-spec export(digraph:digraph(), [option()]) -> iolist().
export(Digraph, Options) ->
    Name = proplists:get_value(name, Options, ""),
    AttrDict = proplists:get_value(attr_dict, Options, dict:new()),
    Nodes =
        [node_stmt(N, AttrDict, Digraph) || N <- digraph:vertices(Digraph)],
    Edges =
        [edge_stmt(N, AttrDict, Digraph) || N <- digraph:edges(Digraph)],
    ["digraph \"", Name, "\" {\n", Nodes, Edges, "}\n"].

-spec export(digraph:digraph(), [option()], string(), string()) ->
                    ok | {error, any()}.
export(Digraph, Options, Filename, Format) ->
    Str = export(Digraph, Options),
    TempFile = lib:nonl(os:cmd("mktemp")),
    Cmd = io_lib:format("dot -T~s ~s > ~s", [Format, TempFile, Filename]),
    case file:write_file(TempFile, Str) of
        {error, Reason} ->
            {error, Reason};
        _ ->
            case os:cmd(Cmd) of
                [] ->
                    file:delete(TempFile);
                Out ->
                    {error, {"dot unexpectedly produced output", Out}}
            end
    end.

%%====================================================================
%% Internal functions
%%====================================================================

attr_clause(Elem, AttrDict, Label) ->
    AttrList = dict_get(Elem, AttrDict, []),
    AttrList_ =
        case Label of
            [] ->
                AttrList;
            _ ->
                LabelStr = io_lib:format("~p", [Label]),
                [{label, LabelStr} | AttrList]
        end,
    AttrStrings =
        [lists:flatten([atom_to_list(K), "=", format(V)])
         || {K, V} <- AttrList_],
    ["[", lists:join(", ", AttrStrings), "]"].

dict_get(Key, Dict, Default) ->
    case dict:find(Key, Dict) of
        {ok, Value} ->
            Value;
        error ->
            Default
    end.

edge_stmt(E, AttrDict, Digraph) ->
    {_, V1, V2, Label} = digraph:edge(Digraph, E),
    Attr = attr_clause(E, AttrDict, Label),
    [node_id(V1), " -> ", node_id(V2), " ", Attr, ";\n"].

format(Term) when is_list(Term) ->
    io_lib:format("~p", [lists:flatten(Term)]);
format(Term) ->
    io_lib:format("~w", [Term]).

node_id(N) ->
    ["\"", io_lib:format("~w", [N]), "\""].

node_stmt(N, AttrDict, Digraph) ->
    {_, Label} = digraph:vertex(Digraph, N),
    Attr = attr_clause(N, AttrDict, Label),
    [node_id(N), " ", Attr, ";\n"].
