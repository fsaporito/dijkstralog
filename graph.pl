% Dynamic Predicates Definitions
:- dynamic graph/1.   % Definition Of Graph
:- dynamic vertex/2.  % Definition Of Vertex
:- dynamic arc/3.     % Definition Of Arc With Weight 1
:- dynamic arc/4.     % Definition Of Arc With Variable Weight



%%%%    new_graph/1    %%%%

% Create The New Graph G In The Prolog Rules' Database
new_graph(G) :- % If G Is Already A Graph, Do Nothing
	graph(G),
	!.

new_graph(G) :- % Add graph(G) To The Database
	assert(graph(G)),
	!.




%%%% delete_graph/1    %%%%

% Delete The Graph G
delete_graph(G) :-
	graph(G),
	retractall(arc(G,_,_,_)), % Remove All The Arcs
	retractall(vertex(G,_)), % Remove All The Vertices
	retractall(graph(G)), % Remove The Graph G
	!.



%%%%    add_vertex/2    %%%%

% Add Vertex V To Graph G
add_vertex(G, V) :- % If The Vertex Already Exists, Do Nothing
	nonvar(G),
	nonvar(V),
	graph(G),
	vertex(G, V),
	!.

add_vertex(G, V) :- % Add vertex(G, V) To The Database
	assert(vertex(G, V)),
	!.



%%%%    vertices/2    %%%%

% Verify If Vs Is A List Containing All G's Vertices
vertices(G, Vs) :-
	nonvar(G),
	graph(G),
	findall(V, vertex(G, V), Ws),
	Vs = Ws.



%%%%    list_vertices/1    %%%%

% Output All Of G's Vertices
list_vertices(G) :-
	nonvar(G),
	graph(G),
	listing(vertex(G, _)).



%%%%    add_arc/3    %%%%

% Add An Arc To The Graph G Between The Vertices U And V, With Weight
add_arc(G, U, V) :-  % add_arc/3, Add Arc With Weight 1
	add_arc(G, U, V, 1),
	!.

add_arc(G, U, V, Weight) :-  % If The Arc Already Exists, Update It's Weight
	nonvar(G),
	nonvar(U),
	nonvar(V),
	nonvar(Weight),
	graph(G),
	arc(G, U, V, _),
	retractall(arc(G, U, V, _)),
	add_arc(G, U, V, Weight),
	!.


add_arc(G, U, V, Weight) :-  % Add arc(G, U, V, Weight) To The Database
	assert(arc(G, U, V, Weight)),
	!.



%%%%    arcs/2    %%%%

% Verify If Es Is A List Containing All G's Arcs
arcs(G, Es) :-
	nonvar(G),
	graph(G),
	Arc = arc(G, _, _, _),
	findall(Arc, Arc, Arcs),
	Es = Arcs.


%%%%    neighbors/3    %%%%

% Verify If Ns Is A List Containing All Vertex V's Neighbors
neighbors(G, V, Ns) :-
	nonvar(G),
	nonvar(V),
	graph(G),
	vertex(G, V),
	Neighbor = arc(G, V, _, _),
	findall(Neighbor, Neighbor, Neighbors),
	Ns = Neighbors.



%%%%    list_arcs/1    %%%%

% Output All Of G's Arcs
list_arcs(G) :-
	nonvar(G),
	graph(G),
	listing(arc(G, _, _, _)).



%%%%    list_graph/1    %%%%

% Output All Of G's Vertices And Arcs
list_graph(G) :-  % Calls list_vertices/1 And list_arcs/1
	nonvar(G),
	graph(G),
	list_vertices(G),
	list_arcs(G).


