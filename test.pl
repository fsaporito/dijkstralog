% Saporito Francesco 763855

% graph 1

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
:- vertices(g1, [v1, v2, v3]).
:- arcs(g1, [arc(g1, v1, v2, 1), arc(g1, v2, v3, 5), arc(g1, v1, v3, 2), arc(g1, v3, v1, 1)]).
:- neighbors(g1, v1, [arc(g1, v1, v2, 1), arc(g1, v1, v3, 2)]).


% Delete Graph 1
/*:- delete_graph(g1).
:- listing(arc(g1,_,_,_)).
:- listing(vertex(g1,_)).
:- listing(graph(g1)).
*/

%sssp(g1, v1).
%shortest_path(g1, v1, v3, [arc(g1, v1, v3, 2)]).
