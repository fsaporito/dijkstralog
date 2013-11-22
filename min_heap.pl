% Saporito Francesco 763855


% Dynamic Predicates Definitions
:- dynamic heap/2.   % Definition Of Heap
:- dynamic heap_entry/4.   % Definition Of Heap Node



% Create New Heap Of Size 0
new_heap(H) :-  % If The Heap H Already Exists In The Database, Do Nothing
	heap(H, _),
	!.

new_heap(H) :-  % Add The Heap H With Size Zero To The Database
	assert(heap(H, 0)),
	!.



% Delete The Heap H
delete_heap(H) :-  % Heapsize = 0 [No Elements To Remove]
	heap(H, 0),
	retract(heap(H, 0)),
	!.

delete_heap(H) :-  % Heapsize = 1 [Head Is The Only Heap's Element]
	heap(H, 1),
	retract(head(H, _, _)),
	retract(heap(H, 1)),
	!.

delete_heap(H) :-  % Heapsize > 1 [Remove All The Heap's Elements]
	heap(H, _),
	retractall(heap_entry(H, _, _, _)),
	retract(heap(H, _)),
	!.



% True If S Is The Actual Size Of THe Heap H
heap_size(H, S) :-
	heap(H, S).



% True If The Heap Is Empty
empty(H) :-   % Empty If The Heap' Size Is Zero
	nonvar(H),
	heap(H, 0).



% True If The Heap Isn't Empty
not_empty(H) :-   % Simply Calls empty(H) And Negates The Boolean Value
	\+ empty(H).



% Node V Is The Head Of The Heap
head (H, K, V) :-  % If V Is The Head, Then It Is At The First Position
	nonvar(H),
	nonvar(K),
	heap(H, _),
	heap_entry(H, 0, K, V).


% Insert Node Into The Heap
insert(H, K, V) :-
	nonvar(H),
	nonvar(K),
	nonvar(V),
	heap(H, S),
	P is S,
	heap_entry(H, P, K, V).

