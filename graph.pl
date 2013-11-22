% Saporito Francesco 763855


% Dynamic Predicates Definitions
:- dynamic graph/1.   % Definition Of Graph
:- dynamic vertex/2.   % Definition Of Vertex
:- dynamic arc/3.       % Definition Of Arc With Weight 1
:- dynamic arc/4.       % Definition Of Arc With Variable Weight



% Create The New Graph G In The Prolog Rules' Database
new_graph(G) :-
	graph(G),
	!.

new_graph(G) :-
	assert(graph(G)),
	!.



% Delete The Graph G
delete_graph(G) :-
	graph(G),
	retractall(arc(G,_,_,_)),
	retractall(vertex(G,_)),
	retractall(graph(G)),
	!.



% Add Vertex V To Graph G
add_vertex(G, V) :-
	nonvar(G),
	nonvar(V),
	graph(G),
	vertex(G, V),
	!.

add_vertex(G, V) :-
	assert(vertex(G, V)),
	!.



% Verify If Vs Is A List Containing All G's Vertices
vertices(G, Vs) :-
	nonvar(G),
	nonvar(Vs),
	graph(G),
	findall(V, vertex(G, V), Ws),
	Vs = Ws.



% Output A List Containing All G's Vertices
list_vertices(G) :-
	nonvar(G),
	graph(G),
	listing(vertex(G, _)).



% Add An Arc To The Graph G Between The Vertices U And V, With Weight
add_arc(G, U, V) :-
	add_arc(G, U, V, 1).

add_arc(G, U, V, Weight) :-
	nonvar(G),
	nonvar(U),
	nonvar(V),
	nonvar(Weight),
	graph(G),
	arc(G, U, V, X),
	retractall(arc(G, U, V, X)),
	add_arc(G, U, V, Weight),
	!.


add_arc(G, U, V, Weight) :-
	assert(arc(G, U, V, Weight)),
	!.



% Verify If Es Is A List Containing All G's Arcs
arcs(G, Es) :-
	nonvar(G),
	nonvar(Es),
	graph(G),
	K = arc(G, _, _, _),
	findall(K, K, Zs),
	Es = Zs.



% Verify If Ns Is A List Containing All Vertex V's Neighbors
neighbors(G, V, Ns) :-
	nonvar(G),
	nonvar(V),
	%nonvar(Ns),
	graph(G),
	vertex(G, V),
	K = arc(G, V, _, _),
	findall(K, K, Zs),
	Ns = Zs.



% Output A List Containing All G's Arcs
list_arcs(G) :-
	nonvar(G),
	graph(G),
	listing(arc(G, _, _, _)).



% Output A List Containing All G's Vertices AndArcs
list_graph(G) :-
	nonvar(G),
	graph(G),
	list_vertices(G),
	list_arcs(G).




