g=new_graph();
n1=graph_add_node(g, 23);
n2=graph_add_node(g, 44);
pedges = fn (l)
  format("%s", lmap(fn (e) format("%s-%s", graph_node_get(graph_edge_from(e)),
				  graph_node_get(graph_edge_to(e))), l));
pnode = fn (n)
  format("%s: %s : %s",
	 graph_node_get(n), pedges(graph_edges_in(n)), pedges(graph_edges_out(n)));
pg = fn (g) lforeach(fn (n) display(format("%s\n", pnode(n))), graph_nodes(g));
n3=graph_add_node(g, 99);
pg(g);
graph_remove_node(n3);
pg(g);
graph_remove_node(n2);
pg(g);
graph_remove_node(n1);
pg(g);
n1=graph_add_node(g, 231);
n2=graph_add_node(g, 441);
n3=graph_add_node(g, 991);
graph_add_edge(n1, n2, 1);
graph_add_edge(n2, n3, 2);
graph_add_edge(n3, n1, 3);
pg(g);
graph_add_edge(n2, n1, 4);
graph_add_edge(n3, n2, 5);
graph_add_edge(n1, n3, 6);
pg(g);
