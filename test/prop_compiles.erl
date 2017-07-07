-module(prop_compiles).

-export([generate_graph_any/0, digraph_add_vertex/2, digraph_add_edge/4]).

-include_lib("proper_eunit.hrl").

digraph_add_edge(Digraph, E, V1, V2) ->
    E = digraph:add_edge(Digraph, E, V1, V2, E),
    Digraph.

digraph_vertices(Digraph) ->
    D = eval(Digraph),
    Vertices = digraph:vertices(D),
    digraph:delete(D),
    Vertices.

digraph_add_vertex(Digraph, V) ->
    V = digraph:add_vertex(Digraph, V, V),
    Digraph.

gen_add_edge(EdgeType, Digraph) ->
    ?LET(E, gen_unused_edge(EdgeType, Digraph),
         begin
             Vertices = digraph_vertices(Digraph),
             ?LET({V1, V2}, {oneof(Vertices), oneof(Vertices)},
                  {'$call', ?MODULE, digraph_add_edge, [Digraph, E, V1, V2]}
                 )
         end
        ).

gen_add_vertex(VertexType, Digraph) ->
    ?LET(V, VertexType,
         {'$call', ?MODULE, digraph_add_vertex, [Digraph, V]}
        ).

gen_unused_edge(EdgeType, Digraph) ->
    ?SUCHTHAT(E, EdgeType,
              begin
                  D = eval(Digraph),
                  Edges = digraph:edges(D),
                  digraph:delete(D),
                  not lists:member(E, Edges)
              end
             ).

generate_graph(VertexType, EdgeType) ->
    ?SIZED(
       Size,
       generate_graph_(VertexType, EdgeType, Size)
      ).

generate_graph_(_, _, 0) ->
    {'$call', digraph, new, [[]]};
generate_graph_(VertexType, EdgeType, Size) ->
    ?LET(Dig, generate_graph_(VertexType, EdgeType, Size-1),
         case digraph_vertices(Dig) of
             [] ->
                 gen_add_vertex(VertexType, Dig);
             _ ->
                 oneof([gen_add_vertex(VertexType, Dig),
                        gen_add_edge(EdgeType, Dig)])
         end
        ).

generate_graph_any() ->
    ?LET(G, generate_graph(any(), any()), G).

prop_compiles() ->
    ?FORALL(Digraph, generate_graph_any(),
            begin
                Result = digraph_viz:export(Digraph, [], "/dev/null"),
                io:format("~p~n", [Result]),
                collect(
                  begin
                      VLen = digraph:no_vertices(Digraph),
                      ELen = digraph:no_edges(Digraph),
                      Class = {{vertices, to_range(VLen, 10)},
                               {edges, to_range(ELen, 10)}},
                      digraph:delete(Digraph),
                      Class
                  end,
                  ok == Result
                 )
            end
           ).

to_range(N, R) ->
    {(N div R) * R, ((N div R) + 1) * R - 1}.
