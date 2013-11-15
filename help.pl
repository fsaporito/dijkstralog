% Saporito Francesco 763855


sssp_help('All') :-
	nl,
	nl,
	write(' ############################## '), nl,
	write(' Graph User Interface '), nl,
	write(' ############################## '), nl,
	nl,
	write('  - sssp_help(''new_graph(G)'')'), nl,
	write('  - sssp_help(''delete_graph(G)'')'), nl,
	write('  - sssp_help(''add_vertex(G, V)'')'), nl,
	write('  - sssp_help(''vertices(G, Vs)'')'), nl,
	write('  - sssp_help(''list_vertices(G)'')'), nl,
	write('  - sssp_help(''add_arc(G, U, V)'')'), nl,
	write('  - sssp_help(''add_arc(G, U, V, Weight)'')'), nl,
	write('  - sssp_help(''arcs(G, Es)'')'), nl,
	write('  - sssp_help(''neighbors(G, Es)'')'), nl,
	write('  - sssp_help(''list_arcs(G)'')'), nl,
	write('  - sssp_help(''list_graph(G)'')'), nl,
	nl,
	nl,
	write(' ############################## '), nl,
	write(' Dijikstra User Interface'), nl,
	write(' ############################## '), nl,
	nl.



%  Help New Graph(G)
sssp_help('new_graph(G)') :-
	write('Insert A New Graph G In The Prolog Database'), nl, nl.


%  Help Delete Graph(G)
sssp_help('delete_graph(G)') :-
	write('Remove The Graph G And All Of It''s Vertices And Graph From The Prolog Database'), nl, nl.
	
	
%  Help Add Vertex(G, V)
sssp_help('add_vertex(G, V)') :-
	write('Add The Vertex V To The Graph G'), nl, nl.
	
	
%  Help Add Vertex(G, V)
sssp_help('add_vertex(G, V)') :-
	write('Add The Vertex V To The Graph G'), nl, nl.
	
	
%  Help Vertices(G, Vs)
sssp_help('vertices(G, Vs)') :-
	write('True If Vs Is A List Containing All G''s Vertices'), nl, nl.
	
	
%  Help List Vertices(G)
sssp_help('list_vertices(G)') :-
	write('Outputs A List Containing All G''s Vertices'), nl, nl.
	
	
%  Help Add Arc(G, U, V)
sssp_help('add_arc(G, U, V)') :-
	write('Add An Arc Of Weight 1 Between The Vertices U And V'), nl, nl.
	
	
%  Help Add Arc(G, U, V, Weight)
sssp_help('add_arc(G, U, V)') :-
	write('Add An Arc Of Weight ''Weight'' Between The Vertices U And V'), nl, nl.
	
	
%  Help Arcs(G, Es)
sssp_help('arcs(G, Es)') :-
	write('True If Es Is A List Containing All G''s Arcs'), nl, nl.
	
	
%  Help Neighbors(G, V)
sssp_help('neighbors(G, V)') :-
	write('True If Ns Is A List Containing All V''s Neighbors'), nl, 
	write('i.e. All The Arcs That Connect Directly V To Others Vertices'), nl.

	
%  Help List Arcs(G)
sssp_help('list_arcs(G)') :-
	write('Outputs A List Containing All G''s Arcs'), nl, nl.
	

%  Help List Graph(G)
sssp_help('list_graph(G)') :-
	write('Outputs A List Containing All G''s Vertices And Arcs'), nl, nl.

