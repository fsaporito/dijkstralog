% Dynamic Predicates Definitions
:- dynamic dist/3.       % Definition Of Predicate Dist
:- dynamic visited/2.   % Definition Of Predicate Visited
:- dynamic previous/3.  % Definition Of Predicate Previous



%%%%    max_int/1    %%%%

% Used To Calculate A Big Value To Be Used As Infinity In The
% Dijkstra Algorithm Initialisation
max_int(Max) :-
	Max is 42^42,
	!.




%%%%    list_visited/1    %%%%

% Output All Of G's Visited Vertices
list_visited(G) :-
	nonvar(G),
	graph(G),
	listing(visited(G, _)),
	!.




%%%%    list_dist/1    %%%%

% Output All Of The Current dist(G, V, D) Entries
list_dist(G) :-
	nonvar(G),
	graph(G),
	listing(dist(G, _, _)),
	!.



%%%%    list_previous/1    %%%%

% Output All Of The Current previous(G, V, U) Entries
list_previous(G) :-
	nonvar(G),
	graph(G),
	listing(previous(G, _, _)),
	!.




%%%%    change_dist/3    %%%%

% If G Is A Graph And V Is A Vertex Of G, Remove All The Current
% Instances Of dist(G, V, _) And Add dist(G, V, NewDist)
change_dist(G, V, NewDist) :-
	nonvar(G),
	nonvar(V),
	nonvar(NewDist),
	graph(G),
	vertex(G, V),
	retractall(dist(G, V, _)),
	assert(dist(G, V, NewDist)),
	!.



%%%%    change_previous/3    %%%%

% If G Is A Graph And V And U Are Vertices Of G, Remove All The Current
% Instances Of previous(G, V, _) From The Database And Add
% previous(G, V, U)
change_previous(G, V, U) :-
	nonvar(G),
	nonvar(V),
	nonvar(U),
	graph(G),
	vertex(G, V),
	vertex(G, U),
	retractall(previous(G, V, _)),
	assert(previous(G, V, U)),
	!.




%%%%    initialise_single_source/4    %%%%

% Initialise All The Vertices Distance From The Source To "Infinity" And
% Previous To "Null"
initialise_single_source(G, Source, Heap, Vs) :-  % Vs = [], Do Nothing
	nonvar(G),
	nonvar(Source),
	nonvar(Heap),
	nonvar(Vs),
	graph(G),
	vertex(G, Source),
	heap(Heap, _),
	Vs = [],
	!.

initialise_single_source(G, Source, H, [V | Vs]) :- % Iterate Over The List
	nonvar(G),
	nonvar(Source),
	nonvar(H),
	nonvar(V),
	nonvar(Vs),
	graph(G),
	vertex(G, Source),
	heap(H, _),
	[V | Vs] \= [],
	vertex(G, V),
	max_int(Max),
	insert(H, Max, V),
	assert(dist(G, V, Max)),
	assert(previous(G, V, null)),
	initialise_single_source(G, Source, H, Vs),
	!.



%%%%    dijkstra_neighbors/4    %%%%

% Update The Distance Of All The Neighbors Of V
dijkstra_neighbor(G, V, H, Ns) :-  % Heap Empty, Do Nothing
	nonvar(G),
	nonvar(V),
	nonvar(H),
	nonvar(Ns),
	graph(G),
	vertex(G, V),
	heap(H, _),
	empty(H),
	!.

dijkstra_neighbor(G, V, H, Ns) :-  % Neighborhood Empty, Do Nothing
	nonvar(G),
	nonvar(V),
	nonvar(H),
	nonvar(Ns),
	graph(G),
	vertex(G, V),
	heap(H, _),
	not_empty(H),
	Ns = [],
	!.


dijkstra_neighbor(G, V, H, [N | Ns]) :-  % V Already Extracted, Do Nothing
	nonvar(G),
	nonvar(V),
	nonvar(H),
	nonvar(N),
	nonvar(Ns),
	graph(G),
	vertex(G, V),
	heap(H, _),
	not_empty(H),
	[N | Ns] \= [],
	arc(G, V, U, _) = N,
	dist(G, V, _),
	\+ heap_entry(H, _, _, U),
	dijkstra_neighbor(G, V, H, Ns),
	!.



dijkstra_neighbor(G, V, H, [N | Ns]) :-  % OldKey > D+Weight
	nonvar(G),
	nonvar(V),
	nonvar(H),
	nonvar(N),
	nonvar(Ns),
	graph(G),
	vertex(G, V),
	heap(H, _),
	not_empty(H),
	[N | Ns] \= [],
	arc(G, V, U, Weight) = N,
	dist(G, V, D),
	heap_entry(H, _, OldKey, U),
	NewKey is (D + Weight),
	OldKey > NewKey,
	modify_key(H, NewKey, OldKey, U),
	change_dist(G, U, NewKey),
	change_previous(G, U, V),
	dijkstra_neighbor(G, V, H, Ns),
	!.

dijkstra_neighbor(G, V, H, [N | Ns]) :-  % OldKey <= D+Weight
	nonvar(G),
	nonvar(V),
	nonvar(H),
	nonvar(N),
	nonvar(Ns),
	graph(G),
	vertex(G, V),
	heap(H, _),
	not_empty(H),
	[N | Ns] \= [],
	arc(G, V, U, Weight) = N,
	dist(G, V, D),
	heap_entry(H, _, OldKey, U),
	NewKey is (D + Weight),
	OldKey =< NewKey,
	dijkstra_neighbor(G, V, H, Ns),
	!.





%%%%    dijkstra/3    %%%%

% Calls dijkstra_neighbor Recursively For Every Node To Find
% The Shortest Path Between Each One Of Them And The Source
dijkstra(G, V, H) :-  % Heap Is Empty, Do Nothing
	nonvar(G),
	nonvar(V),
	nonvar(H),
	graph(G),
	vertex(G, V),
	heap(H, _),
	empty(H),
	!.

dijkstra(G, V, H) :-  % Heap With Only One Element
	nonvar(G),
	nonvar(V),
	nonvar(H),
	graph(G),
	vertex(G, V),
	heap(H, S),
	not_empty(H),
	S = 1,
	neighbors(G, V, Ns),
	dijkstra_neighbor(G, V, H, Ns),
	assert(visited(G, V)),
	dist(G, V, D),
	extract(H, D, V),
	!.

dijkstra(G, V, H) :-  % Call On A General Vertex V
	nonvar(G),
	nonvar(V),
	nonvar(H),
	graph(G),
	vertex(G, V),
	heap(H, S),
	not_empty(H),
	S \= 1,
	neighbors(G, V, Ns),
	dijkstra_neighbor(G, V, H, Ns),
	assert(visited(G, V)),
	dist(G, V, D),
	extract(H, D, V),
	head(H, _, U),
	dijkstra(G, U, H),
	!.

