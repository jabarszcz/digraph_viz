-module(digraph_viz).

%% API exports
-export([
    dot_string/1,
    dot_string/2,
    export/3,
    export/4
  ]).

-export_types([
    option/0
  ]).

%%====================================================================
%% API types
%%====================================================================

-type digraph_elem() :: {vertex, digraph:vertex()} | {edge, digraph:edge()}.

-type option() :: {name, iolist()}
                | {attr_dict, dict:dict(digraph_elem(), {atom(), iolist()})}
                | {label_callback, fun((digraph_elem()) -> iolist())} .

%%====================================================================
%% API functions
%%====================================================================

-spec dot_string(digraph:digraph()) -> iolist().
dot_string(Digraph) ->
    dot_string(Digraph, []).

-spec dot_string(digraph:digraph(), [option()]) -> iolist().
dot_string(Digraph, Options) ->
    Name = proplists:get_value(name, Options, ""),
    Nodes =
        [node_stmt(N, Digraph, Options) || N <- digraph:vertices(Digraph)],
    Edges =
        [edge_stmt(N, Digraph, Options) || N <- digraph:edges(Digraph)],
    ["digraph ", escape_quotes(Name), " {\n", Nodes, Edges, "}\n"].

-spec export(digraph:digraph(), [option()], string()) ->
                    ok | {error, any()}.
export(Digraph, Options, Filename) ->
    export(Digraph, Options, Filename, "png").

-spec export(digraph:digraph(), [option()], string(), string()) ->
                    ok | {error, any()}.
export(Digraph, Options, Filename, Format) ->
    Str = dot_string(Digraph, Options),
    TempFile = lib:nonl(os:cmd("mktemp")),
    Cmd = io_lib:format(
            "dot -Gcharset=latin1 -T~s ~s > ~s",
            [Format, escape_quotes(TempFile), escape_quotes(Filename)]
           ),
    case file:write_file(TempFile, Str) of
        {error, Reason} ->
            {error, Reason};
        _ ->
            Out = os:cmd(Cmd),
            case classify_output(Cmd, Out) of
                ok ->
                    file:delete(TempFile);
                Err ->
                    Err
            end
    end.

%%====================================================================
%% Internal functions
%%====================================================================

attr_clause(Elem, Digraph, Options) ->
    AttrDict = proplists:get_value(attr_dict, Options, dict:new()),
    AttrList = dict_get_value(Elem, AttrDict, []),
    AttrList_ =
        case label_str(Elem, Digraph, Options) of
            undefined ->
                AttrList;
            LabelStr ->
                [{label, escape_quotes(LabelStr)} | AttrList]
        end,
    AttrStrings =
        [lists:flatten([atom_to_list(K), "=", V])
         || {K, V} <- AttrList_],
    ["[", lists:join(", ", AttrStrings), "]"].

classify_output(Cmd, Out) ->
    Lines = re:split(Out, "\n", [{return, list}, trim]),
    case lists:all(fun classify_output_line_ok/1, Lines) of
        true ->
            ok;
        _ ->
            {error,
             {"command unexpectedly produced output",
              lists:flatten(Cmd),
              lists:flatten(Out)
             }
            }
    end.

classify_output_line_ok(Line) ->
    re:run(Line, "dot: graph is too large for .* Scaling by [0-9.]+ to fit",
           [{capture, none}]) == match orelse
        Line =:= "".

dict_get_value(Key, Dict, Default) ->
    case dict:find(Key, Dict) of
        {ok, Value} ->
            Value;
        error ->
            Default
    end.

edge_stmt(E, Digraph, Options) ->
    {_, V1, V2, _} = digraph:edge(Digraph, E),
    Attr = attr_clause({edge, E}, Digraph, Options),
    [node_id(V1), " -> ", node_id(V2), " ", Attr, ";\n"].

escape_quotes(Str) ->
    Escaped =
        re:replace(Str, "([\\\\\"])", "\\\\\\g1", [global, {return, list}]),
    ["\"", Escaped, "\""].

label_str(Elem, Digraph, Options) ->
    case proplists:get_value(label_callback, Options) of
        undefined ->
            Label =
                case Elem of
                    {vertex, V} ->
                        {_, VLabel} = digraph:vertex(Digraph, V),
                        VLabel;
                    {edge, E} ->
                        {_, _, _, ELabel} = digraph:edge(Digraph, E),
                        ELabel
                end,
            case Label of
                [] -> undefined;
                _ -> io_lib:format("~w", [Label])
            end;
        Function ->
            Function(Elem)
    end.

node_id(N) ->
    escape_quotes(io_lib:format("~w", [N])).

node_stmt(N, Digraph, Options) ->
    Attr = attr_clause({vertex, N}, Digraph, Options),
    [node_id(N), " ", Attr, ";\n"].
