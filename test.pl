

% graph
:- nl.
:- nl.
:- write(' ############################## '), nl.
:- write(' Graphs '), nl.
:- write(' ############################## '), nl, nl, nl.


% Graph 1
:- write(' - New Graph g1 ').
:- new_graph(g1).
:- write(' :   OK '), nl.

% Graph 1 Vertices
:- write(' - Vertices '), nl.

:- write('		v1 ').
:- add_vertex(g1, v1).
:- write(' :   OK '), nl.

:- write('		v2 ').
:- add_vertex(g1, v2).
:- write(' :   OK '), nl.

:- write('		v3 ').
:- add_vertex(g1, v3).
:- write(' :   OK '), nl.


% Graph 1 Arcs
:- write(' - Arcs '), nl.

:- write('		v1 - v2 (1) ').
:- add_arc(g1, v1, v2, 1).
:- write(' :   OK '), nl.

:- write('		v2 - v3 (5) ').
:- add_arc(g1, v2, v3, 5).
:- write(' :   OK '), nl.

:- write('		v1 - v3 (2) ').
:- add_arc(g1, v1, v3, 2).
:- write(' :   OK '), nl.

:- write('		v3 - v1 (1) ').
:- add_arc(g1, v3, v1, 1).
:- write(' :   OK '), nl.


% Print Graph 1 Features
:- list_graph(g1).


% Controls Over Correct Istantation
:- list_vertices(g1).
:- vertices(g1, [v1, v2, v3]).
:- arcs(g1, [arc(g1, v1, v2, 1), arc(g1, v2, v3, 5), arc(g1, v1, v3, 2), arc(g1, v3, v1, 1)]).
:- neighbors(g1, v1, [arc(g1, v1, v2, 1), arc(g1, v1, v3, 2)]).


% Delete Graph 1
:- delete_graph(g1).
:- listing(arc(g1,_,_,_)).
:- listing(vertex(g1,_)).
:- listing(graph(g1)).





% Heap h1
:- nl.
:- nl.
:- write(' ############################## '), nl.
:- write(' Heap 1'), nl.
:- write(' ############################## '), nl, nl, nl.


:- new_heap(h1).
:- heap_size(h1, _).
:- empty(h1).
:- insert(h1, 30, v1).
:- not_empty(h1).
:- head(h1, 30, v1).
:- head(h1, _, _).
:- list_heap(h1).
:- modify_key(h1, 345, 30, v1).
:- heap_size(h1, 1).
:- list_heap(h1).
:- insert(h1, 21, v1).

:- head(h1, 21, v1).
:- list_heap(h1).
:- not_empty(h1).

:- extract(h1, 21, v1).
:- empty(h1).
:- list_heap(h1).

:- insert(h1, 1, v1).
:- insert(h1, 23, v1).
:- insert(h1, 13, v1).
:- list_heap(h1).
:- insert(h1, 10, v2).
:- list_heap(h1).
:- insert(h1, 231, v3).
:- head(h1, 10, v2).
:- list_heap(h1).

:- insert(h1, 4, v1).
:- insert(h1, 9, v4).
:- insert(h1, 17, v5).
:- insert(h1, 88, v6).
:- list_heap(h1).

:- extract(h1, 4, v1).
:- list_heap(h1).

:- delete_heap(h1).
:- list_heap(h1).







% Heap h2
:- nl.
:- nl.
:- write(' ############################## '), nl.
:- write(' Heap 2'), nl.
:- write(' ############################## '), nl, nl, nl.

:- new_heap(h).
:- insert(h, 0, a).
:- insert(h, 88, b).
:- insert(h, 77, c).
:- insert(h, 178, d).
:- list_heap(h).
:- extract(h, 0, a).
:- list_heap(h).
:- delete_heap(h).





% Dijkstra
:- nl.
:- nl.
:- nl.
:- write(' ############################## '), nl.
:- write(' Dijkstra'), nl.
:- write(' ############################## '), nl, nl, nl.


:- new_graph(g2).
:- add_vertex(g2, v1).
:- add_vertex(g2, v2).
:- add_vertex(g2, v3).
:- add_vertex(g2, v4).
:- add_vertex(g2, v5).
:- add_arc(g2, v1, v2, 88).
:- add_arc(g2, v1, v3, 77).
:- add_arc(g2, v1, v5, 178).
:- add_arc(g2, v2, v3, 3).
:- add_arc(g2, v2, v4, 74).
:- add_arc(g2, v3, v5, 42).
:- add_arc(g2, v4, v5, 8).
:- list_graph(g2).

:- nl.

:- sssp(g2, v1).

:- list_visited(g2).
:- list_dist(g2).
:- list_previous(g2).

:- shortest_path(g2, v1, v2, [arc(g2, v1, v2, 88)]).
:- shortest_path(g2, v1, v3, [arc(g2, v1, v3, 77)]).
:- shortest_path(g2, v1, v4, [arc(g2, v1, v2, 88), arc(g2, v2, v4, 74)]).
:- shortest_path(g2, v1, v5, [arc(g2, v1, v3, 77), arc(g2, v3, v5, 42)]).

%:- shortest_path(g2, v1, v2, P), nl, write(P).

% Delete Graph 2
:- delete_graph(g2).




