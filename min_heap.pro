% Dynamic Predicates Definitions
:- dynamic heap/2.         % Definition Of Heap
:- dynamic heap_entry/4.   % Definition Of Heap Node



%%%%    new_heap/1    %%%%

% Create New Heap Of Size 0
new_heap(H) :-  % If The Heap H Already Exists In The Database, Do Nothing
	heap(H, _),
	!.

new_heap(H) :-  % Add The Heap H With Size Zero To The Database
	assert(heap(H, 0)),
	!.




%%%%    delete_heap/1    %%%%

% Delete The Heap H
delete_heap(H) :-  % Heapsize = 0 [No Elements To Remove]
	heap(H, 0),
	retract(heap(H, 0)),
	!.

delete_heap(H) :-  % Heapsize >= 1 [Remove All The Heap's Elements]
	heap(H, _),
	retractall(heap_entry(H, _, _, _)),
	retract(heap(H, _)),
	!.




%%%%    heap_size/2    %%%%

% True If S Is The Actual Size Of The Heap H
heap_size(H, S) :-
	heap(H, S),
	!.




%%%%    empty/1    %%%%

% True If The Heap Is Empty
empty(H) :-   % Empty If The Heap'Size Is Zero
	nonvar(H),
	heap(H, 0),
	!.




%%%%    not_empty/1    %%%%

% True If The Heap Isn't Empty
not_empty(H) :-   % Not Empty If The Heap'Size Is More Than Zero
	nonvar(H),
	heap(H, S),
	S > 0.




%%%%    head/3    %%%%

% Node V Is The Head Of The Heap
head(H, K, V) :-  % If V Is The Head, Then It Is At The First Position
	nonvar(H),
	heap(H, _),
	heap_entry(H, 1, K, V),
	!.




%%%%%    swap/7    %%%%

% Swap The Two Heap Entry:
% heap_entry (H, P1, K1, V1)
% heap_entry (H, P2, K2, V2)
swap(H, P1, K1, V1, P2, K2, V2) :-
	nonvar(H),
	nonvar(P1),
	nonvar(K1),
	nonvar(V1),
	nonvar(P2),
	nonvar(K2),
	nonvar(V2),
	heap_entry(H, P1, K1, V1),
	heap_entry(H, P2, K2, V2),
	retract(heap_entry(H, P1, K1, V1)),
	retract(heap_entry(H, P2, K2, V2)),
	assert(heap_entry(H, P2, K1, V1)),
	assert(heap_entry(H, P1, K2, V2)),
	!.




%%%%    minimum/4    %%%%

% True If Min Is The Minimum Value Between Position
% One And Two
minimum(H, Pone, Ptwo, Min) :- % Kone < Ktwo
	nonvar(H),
	nonvar(Pone),
	nonvar(Ptwo),
	heap(H, S),
	Pone =< S,
	Ptwo =< S,
	heap_entry(H, Pone, Kone, _),
	heap_entry(H, Ptwo, Ktwo, _),
	Kone < Ktwo,
	Min is Pone,
	!.

minimum(H, Pone, Ptwo, Min) :- % Kone >= Ktwo
	nonvar(H),
	nonvar(Pone),
	nonvar(Ptwo),
	heap(H, S),
	Pone =< S,
	Ptwo =< S,
	heap_entry(H, Pone, Kone, _),
	heap_entry(H, Ptwo, Ktwo, _),
	Kone >= Ktwo,
	Min is Ptwo,
	!.




%%%%    minimum/5    %%%%%

% True If Min Is The Minimum Value Between Position
% One ,Two And Three
minimum(H, Pone, Ptwo, Pthree, Min) :- % K(Min1) < K(Min2)
	nonvar(H),
	nonvar(Pone),
	nonvar(Ptwo),
	nonvar(Pthree),
	heap(H, S),
	Pone =< S,
	Ptwo =< S,
	Pthree =< S,
	minimum(H, Pone, Ptwo, Min1),
	minimum(H, Ptwo, Pthree, Min2),
	heap_entry(H, Min1, K1, _),
	heap_entry(H, Min2, K2, _),
	K1 < K2,
	Min is Min1,
	!.

minimum(H, Pone, Ptwo, Pthree, Min) :- % K(Min1) >= K(Min2)
	nonvar(H),
	nonvar(Pone),
	nonvar(Ptwo),
	nonvar(Pthree),
	heap(H, S),
	Pone =< S,
	Ptwo =< S,
	Pthree =< S,
	minimum(H, Pone, Ptwo, Min1),
	minimum(H, Ptwo, Pthree, Min2),
	heap_entry(H, Min1, K1, _),
	heap_entry(H, Min2, K2, _),
	K1 >= K2,
        Min is Min2,
	!.




%%%%    heapify/2    %%%%

% Heapify, Maintains The MinHeap Structure
% By Checking If The Root Is The Minimum Key And Swapping
% It With The Minimum Of Its Sons Otherwise. It Then Recalls
% heapify On The Position Of The Minimum Of The Sons
heapify(H, _) :-  % If The Heap Is Empty, Do Nothing
	nonvar(H),
	empty(H),
	!.

heapify(H, _) :-  % If The Heap Has Only One Element, Do Nothing
	nonvar(H),
	not_empty(H),
	heap(H, 1),
	!.

heapify(H, Node) :-  % If Node Position Is Bigger Than S, Do Nothing
	nonvar(H),
	not_empty(H),
	heap(H, S),
	S < Node,
	!.

heapify(H, Node) :- % Node With Zero Sons, Do Nothing
	nonvar(H),
	heap(H, S),
	S \= 0,
	S \= 1,
	Node =< S,
	Node =< S,
	Left is 2*Node,
	Left > S,
	Right is ((2*Node)+1),
	Right > S,
	!.

heapify(H, Node) :- % Node With One Son, Node Is The Min
	nonvar(H),
	heap(H, S),
	S \= 0,
	S \= 1,
	Node =< S,
	Left is 2*Node,
	Left =< S,
	Right is ((2*Node)+1),
	Right > S,
	heap_entry(H, Left, _, _),
	heap_entry(H, Left, Kleft, _),
	write('Kleft = '), write(Kleft), nl, nl,
	heap_entry(H, Node, _, _),
	minimum(H, Node, Left, Min),
	Node = Min,
	heapify(H, Left),
	!.

heapify(H, Node) :- % Node With One Son, Node Isn't The Min
	nonvar(H),
	heap(H, S),
	S \= 0,
	S \= 1,
	Node =< S,
	Left is 2*Node,
	Left =< S,
	Right is ((2*Node)+1),
	Right > S,
	heap_entry(H, Left, _, _),
	heap_entry(H, Node, _, _),
	minimum(H, Node, Left, Min),
	Node \= Min,
	heap_entry(H, Min, Kmin, Vmin),
	heap_entry(H, Node, Knode, Vnode),
	swap(H, Min, Kmin, Vmin, Node ,Knode, Vnode),
	heapify(H, Min),
	!.

heapify(H, Node) :- % Node With Two Sons, Node Is The Min
	nonvar(H),
	heap(H, S),
	S \= 0,
	S \= 1,
	Node =< S,
	Left is 2*Node,
	Left =< S,
	Right is ((2*Node)+1),
	Right =< S,
	heap_entry(H, Left, _, _),
	heap_entry(H, Right, _, _),
	heap_entry(H, Node, _, _),
	minimum(H, Node, Left, Right, Min),
	Node = Min,
	!.

heapify(H, Node) :- % Node With Two Sons, Node Is Not The Min
	nonvar(H),
	heap(H, S),
	S \= 0,
	S \= 1,
	Node =< S,
	Left is 2*Node,
	Left =< S,
	Right is ((2*Node)+1),
	Right =< S,
	heap_entry(H, Left, _, _),
	heap_entry(H, Right, _, _),
	heap_entry(H, Node, _, _),
	minimum(H, Node, Left, Right, Min),
	Node \= Min,
	heap_entry(H, Min, Kmin, Vmin),
	heap_entry(H, Node, Knode, Vnode),
	swap(H, Min, Kmin, Vmin, Node ,Knode, Vnode),
	heapify(H, Min),
	!.



%%%%    pull_up/2    %%%%

% Maintains The MinHeap Structure
% By Checking If The Node Is At The Right Position And Swapping
% It With His Parent. It Then Recalls pull_up
% On The Position Of The Root Of This Subtree
pull_up(H, _) :-  % If The Heap Is Empty, Do Nothing
	nonvar(H),
	heap(H, _),
	empty(H),
	!.

pull_up(H, _) :- % If The Heap Has Only One Element, Do Nothing
	nonvar(H),
	heap(H, S),
	not_empty(H),
	S = 1,
	!.

pull_up(H, Node) :-  % If The Node Is The Root, Do Nothing
	nonvar(H),
	heap(H, S),
	not_empty(H),
	S \= 1,
	Node = 1,
	!.

pull_up(H, Node) :-  % K_Father <= K_Node, Do Nothing
	nonvar(H),
	heap(H, S),
	not_empty(H),
	S \= 1,
	Node \= 1,
	heap_entry(H, Node, _, _),
	Father is floor(Node/2),
	heap_entry(H, Father, _, _),
	minimum(H, Node, Father, Min),
	Min = Father,
	!.

pull_up(H, Node) :-  % K_Father > K_Node, Swap And Recursive Call
	nonvar(H),
	heap(H, S),
	not_empty(H),
	S \= 1,
	Node \= 1,
	heap_entry(H, Node, Knode, Vnode),
	Father is floor(Node/2),
	heap_entry(H, Father, Kfather, Vfather),
	minimum(H, Node, Father, Min),
	Min \= Father,
	swap(H, Node, Knode, Vnode, Father, Kfather, Vfather),
	pull_up(H, Father),
	!.




%%%%    insert/3    %%%%

% Insert Node Into The Heap
insert(H, K, V) :-  % If Already In The Heap, Do Nothing
	nonvar(H),
	nonvar(K),
	nonvar(V),
	heap(H, _),
	not_empty(H),
	heap_entry(H, _, K, V),
	!.

insert(H, K, V) :-  % If Already In The Heap With Different Key, Update It
	nonvar(H),
	nonvar(K),
	nonvar(V),
	heap(H, _),
	not_empty(H),
	heap_entry(H, _, OldKey, V),
        modify_key(H, K, OldKey, V),
	!.

insert(H, K, V) :-  % Add heap_entry To The Database And Update The Heap Size
	nonvar(H),
	nonvar(K),
	nonvar(V),
	heap(H, S),
	Stmp is S+1,
	\+ heap_entry(H, _, K, V),
	retract(heap(H, S)),
	assert(heap(H, Stmp)),
	assert(heap_entry(H, Stmp, K, V)),
	pull_up(H, Stmp),
	!.




%%%%    extract/3    %%%%

% Extract The Head From The Heap, Decrease The HeapSize While
% Mantaining The Heap Structure
extract(H, K, _) :- % If The Heap Is Empty, Do Nothing
	nonvar(H),
	nonvar(K),
	empty(H),
	!.

extract(H, K, V) :-  % If It Isn't The Head, Do Nothing
	nonvar(H),
	nonvar(V),
	heap(H, _),
	not_empty(H),
	\+ head(H, K, V),
	!.

extract(H, K, V) :-  % If The Head Is The Only Element
	nonvar(H),
	nonvar(V),
	heap(H, S),
	not_empty(H),
	S = 1,
	heap_entry(H, S, K, V),
	Stmp is S-1,
	retract(heap_entry(H, S, K, V)),
	retract(heap(H, S)),
	assert(heap(H, Stmp)),
	!.

extract(H, K, V) :-  % Retract The Head And Update The Heap Size
	%write('HI'), nl,
	nonvar(H),
	nonvar(V),
	heap(H, S),
	not_empty(H),
	Stmp is S-1,
	head(H, K, V),
	heap_entry(H, S, Klast, Vlast),
	%list_heap(H),
	swap(H, 1, K, V, S, Klast, Vlast),
	%list_heap(H),
	retract(heap_entry(H, S, K, V)),
	retract(heap(H, S)),
	assert(heap(H, Stmp)),
	%list_heap(H),
	heapify(H, 1),
	!.




%%%%    modify_key/4    %%%%

% Substitute The Value OldKey With NewKey For The Vertex V
%  Mantaining The MinHeap Structure
modify_key(H, NewKey, OldKey, V) :- % Old And New Key Are Equals, Do Nothing
	nonvar(H),
	nonvar(NewKey),
	nonvar(OldKey),
	nonvar(V),
	NewKey = OldKey,
	heap_entry(H, _, OldKey, V),
	!.

modify_key(H, NewKey, OldKey, V) :- % NewKey < OldKey, pull_up
	nonvar(H),
	nonvar(NewKey),
	nonvar(OldKey),
	nonvar(V),
	heap_entry(H, P, OldKey, V),
	NewKey < OldKey,
	retract(heap_entry(H, P, OldKey, V)),
	assert(heap_entry(H, P, NewKey, V)),
	pull_up(H, P),
	!.

modify_key(H, NewKey, OldKey, V) :- % NewKey > OldKey, heapify
	nonvar(H),
	nonvar(NewKey),
	nonvar(OldKey),
	nonvar(V),
	heap_entry(H, P, OldKey, V),
	NewKey > OldKey,
	retract(heap_entry(H, P, OldKey, V)),
	assert(heap_entry(H, P, NewKey, V)),
	heapify(H, P),
	!.





%%%%    list_heap/1    %%%%

% Show The Heap Internal Status
list_heap(H) :- % List All The Heap Entries (heap_entry/4)
	nonvar(H),
	listing(heap_entry(H, _, _, _)).





