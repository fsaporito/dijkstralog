
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%  Graph Implementation  %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- consult('graph.pro').




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%  MinHeap Implementation %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- consult('min_heap.pro').




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%  Dijkstra's Algorithm Implementation %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- consult('dijkstra.pro').



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%  Dijkstra's Algorithm Implementation %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%    sssp/2    %%%%

% Retract All The Existing Entries Of previous, dist And visited,
% Initialise The Heap, Insert All The Vertices And Call The Dijkstra's
% Algorithm On The Source
sssp(G, Source) :-
	nonvar(G),
	nonvar(Source),
	graph(G),
	vertex(G, Source),
	new_heap(h),
	retractall(previous(G, _, _)),
	retractall(dist(G, _, _)),
	retractall(visited(G, _)),
	vertices(G, Vs),
	initialise_single_source(G, Source, h, Vs),
	change_dist(G, Source, 0),
	max_int(Max),
	modify_key(h, 0, Max, Source),
	dijkstra(G, Source, h),
	delete_heap(h),
	!.




%%%%    shortest_path/4    %%%%

% True If Path Is A List Containing All The Arcs In The Shortest Path
% Between Source And V
shortest_path(G, Source, V, Path) :- % If Source And V Coincide, Path = []
	nonvar(G),
	nonvar(Source),
	nonvar(V),
	graph(G),
	vertex(G, Source),
	vertex(G, V),
	Source = V,
	Path = [],
	!.

shortest_path(G, Source, V, Path) :- % Add Recursively All The Arcs
	nonvar(G),
	nonvar(Source),
	nonvar(V),
	graph(G),
	vertex(G, Source),
	vertex(G, V),
	Source \= V,
	previous(G, V, U),
	arc(G, U, V, Weight),
	shortest_path(G, Source, U, Ptmp),
	append(Ptmp, [arc(G, U, V, Weight)], Path),
	!.




%%%%%%%%%%%%%%%%%%%%
%%%% Test File  %%%%
%%%%%%%%%%%%%%%%%%%%

%:- consult('test.pro').



