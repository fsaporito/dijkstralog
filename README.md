dijkstralog
===========

Implementation in prolog of Dijkstra's algorithm for finding the shortest path in a directed , connected and acyclic graph with non-negative weight.


The main file is sssp.pl, which calls the various part of the program:
graph.pro
min_heap.pro
dikstra.pro



===

****  graph.pro  ****

Contatins The API for the implementation in prolog of a graph.
Every graph is defined by a set of facts saved in the prolog's
database:

graph(G) -> The graph named G

vertex(G, V) -> The vertex V of the graph G

arc(G, V, U, Weight) -> Arc between The Vertices V And U, With
			NonNegative Weight


The predicates implemented by the API are the following:

new_graph(G)  ->  Instantiate the graph G in the prolog database.

delete(G)  -> Remove the graph G and all of its vertices and arcs.

add_vertex(G, V)  ->  Add the vertex V of the graph G to the database.

vertices(G, Vs) - > True if Vs is a list containing all of G's vertices.
                    It can be used to obtain all the vertices of G, by
		    using Vs as a variable.

list_vertices(G) -> Output to the screen all of G's vertices.

add_arc(G, V, U, Weight) -> Add the arc between the vertices V and G,
		            with Non-negative weight Weight.

arcs(G, Es) -> True if Es is a list containing all of G's arcs. It can
	       be used to obtain all the arcs of the graph G by
	       using Es as a variable.

neighbors(G, V, Ns) -> True if Es is a list containing all of V's
                       neighbors, i.e. all of the vertices that can be
		       accessed from V directly by walking through one
		       arc. It can be used to obtain all the neighbors
		       of the vertex V by using Ns as a variable.

list_arcs(G) -> Output to the screen all of G's arcs.

list_grap(G)  ->  Output to the screen all of G's vertices and arcs.



===

****  min_heap.pro  ****

Implementation of a min_heap, used to find the minimum value in the
Dijkstra's algorithm by extracting the root of the associated binary
tree.
The heap is representend in the prolog database by the facts:

heap_entry(H, P, k, V)

where H is the heap, P is the position of the considered node, K is the
key of the node used to determinate the position (Every father has a key
less or equal than his sons) and V is the value of the node.
The  API implements the following predicates:

new_heap(H)  ->  Insert the heap H in the database.

delete_heap(H)  ->  Delete the heap H and all the related heap_entries.

heap_size(H, S) -> True if S is the heapsize. It can be used to obtaing
                   the heapsize of the heap H by using S as a variable.

empty(H) -> True if the heap is empty (Heapsize = Zero).

not_empty(H) -> True if the heap has at least one element
               (Heapsize > Zero).

head(H, K, V) -> True if the node with key K and value V is at the first
                 position (K is the minimum key of the heap).

swap(H, P1, K1, V1, P2, K2, V2) -> Swap the position of the two
                                   heap_entries, removing
                                   heap_entry(H, P1, K1, V1)
                                   heap_entry(H, P2, K2, V2)
                                   from the database
                                   and adding
                                   heap_entry(H, P2, K1, V1),
                                   heap_entry(H, P1, K2, V2).
                                   !!! It doesn't mantain the heap
				   structure, it's the caller's
				   responsiility to do so.

minimum(h, Pone, Ptwo, Min) -> True if Min is the minimum key between
                               the key of the node in position Pone and
			       the key in position two. It can e used to
			       obtain the minimum by using Min as a
			       variable.

minimum(h, Pone, Ptwo, Pthree, Min) -> True if Min is the minimum key
                                       between the key of the node in
				       position Pone ,the key in
				       position Ptwo and the key in
				       position Pthree. It works by
				       finding the minimum beween Pone
				       and Ptwo, then between Ptwo And
				       Pthree, using minimum/4. It then
				       find the Min value by comparing
				       the two mins calculated. It can e
				       used to obtain the minimum by
				       using Min as a variable.

heapify(H, Node) -> Mantains the min_heap structure of the subtree of
                   root Node, supposing that the left and the right
		   subtree with roots respectively the left and the
		   right sons of Node, are alreadt ordered as a
		   min-heap. It works by finding the minimum between the
		   node and its sons, swapping it with the node and
		   recalling heapify(H, minimum) to recreate the min
		   heap structure in the subtree that had the min as
		   root, which now is composed by the key and the value
		   of the node.

pull_up(H, Node) -> Mantains the min-heap structure by checking if the
                    node is at the right position comparing it with his
	            father. If the node's key is less than the father's
		    key, swaps node and father and calls pull_up(H,
		    father).

insert(H, K, V) -> If heap_entry(H, _, K, V) doesn't exist in the
                   database, inserts the heap entry at the end of the
		   heap, i.e. heap_entry(H, S, K, V) where S is
		   heapsize+1, and then calls pull_up to mantain the
		   min-heap structure.
                   Otherwise, calls modify_key to change the key to K
		   and to mantain the min-heap structure.

extract(H, K, V) -> Swaps the head(H, K, V) with the last heap_entry,
                    the one in position Heapsize, then remove the head
		    at the new positions and calls heapify at the root
		    level (heapify(H, 1)), to mantain the min-heap.
		    structure.

modify_key(H, NewKey, oldKey, V) -> If OldKey = NewKey, Does Nothing.
                                    Otherwise, it changes the heap_entry
		                    heap_entry(H, P, OldKey, V)
                                    To
			            heap_entry(H, P, NewKey, V).
                                    If OldKey < NewKey, calls
                                    heapify(H, P)
                                    to mantain the min-heap structure.
                                    Otherwise, if OldKey > NewKey, calls
                                    pull_up(H, P)
                                    to mantain the min-heap structure.



===

****  dijkstra.pro  ****

Implementation of the Dijkstra's algorithm for finding the shortest path
between a vertex and all the others.
The iterations over the graph are represented in the prolog database by
the following dynamics predicates:

visited(G, V) -> True if the vertex V as been visited during the
                 execution of the algorithm.
dist(G, V, D) -> True if D is the distance of the vertex V from the
                 Source
previous(G, V, U) -> True if the vertex U is the previous of the vertex
                     V in the shortest path between the Source and the
		     vertex V

The API is composed by te following predicates:

max_int(Max) -> Used to set the distance to an "infinity value". For
                simplicity, Max is unified with 42^42 to use a big
		value.

list_visited(G) -> Outputs to the screen the list of the G's vertex
                   visited during the executionof the algorithm.

list_dist(G) -> Outputs to the screen the list of the G's vertices'
                distances from the Source.

list_previous(G) -> Outputs to the screen the list of the previous
                    entries of the graph G generated during the algoithm
	            execution.

change_dist(G, V, NewDist) -> Sets the distance between the vertex V and
                              the Source to NewDist.

change_previous(G, V, U) -> Sets the vertex U as the previous of the
                            vertex V in the shortest path between the
			    Source and V.

initialise_single_sources(G, Source, Heap, Vs) -> Initialise the
                                                  distance of every
						  vertex in the list Vs
						  to infinity, and every
						  previous to null.

dijkstra_neighbor(G, V, H, Ns) -> Update the distance of every
                                  V's neighbors using the weight of the
			          arcs between V and each neighbor.

dijkstra(G, V, H) -> For every node in the vertex, calls
                     dijkstra_neighbor, assert visited(V) in the
		     database and extracts the heap_entry relative to V
		     from the heap.



===

**** ssp.pro  ****

Main file, wich loads the other parts (with consult), and contains the
two major predicates of the program:

sssp(G, Source) -> Purges the database from previous, dist and visited
                   entries. It then procedes to create a new heap to be
	           used as a min-priority-queue for the Dijkstra's
	           algorithm and inserts all of G's vertices. As
		   last action, it calls the Dijkstra's Algorithm
		   at the Source position.

shortest_path(G, Source, V, Path) -> True if Path is a list containing
                                     all of the arcs in the shortest
				     path between the Source and the
				     vertex V. It supposes that the
				     databases has already been
				     populated with visited, dist and
				     previous entries. It can be used to
				     find the list of arcs in the
				     shortes path between Source and V
				     by using Path as a variable.
