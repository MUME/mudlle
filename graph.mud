/* 
 * Copyright (c) 1993-1999 David Gay
 * All rights reserved.
 * 
 * Permission to use, copy, modify, and distribute this software for any
 * purpose, without fee, and without written agreement is hereby granted,
 * provided that the above copyright notice and the following two paragraphs
 * appear in all copies of this software.
 * 
 * IN NO EVENT SHALL DAVID GAY BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
 * SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OF
 * THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF DAVID GAY HAVE BEEN ADVISED OF
 * THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * DAVID GAY SPECIFICALLY DISCLAIM ANY WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS ON AN "AS IS" BASIS, AND DAVID
 * GAY HAVE NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES,
 * ENHANCEMENTS, OR MODIFICATIONS.
 */

library graph // A generic directed graph type, with support for typical graph algos.
requires system, sequences
defines new_graph, copy_graph, graph_add_node, graph_remove_node,
  graph_add_edge, graph_remove_edge, graph_nodes, graph_nodes_apply,
  graph_node_graph, graph_node_get, graph_node_set!,
  graph_edges_in, graph_edges_out, graph_edges_in_apply, graph_edges_out_apply,
  graph_edge_from, graph_edge_to, graph_edge_get, graph_edge_set!,
  graph_clear_all_marks, graph_mark_node, graph_unmark_node, graph_node_marked?,
  graph_mark_edge, graph_unmark_edge, graph_edge_marked?
[
// A graph is a mutable data structure composed of nodes linked 
// by edges.
//
// Edges can be added and removed between existing nodes, nodes can
// be added at will but only removed if they have no ingoing and no
// outgoing edges.
//
// Each edge & node has associated user data (stored as a (void *))
// and a mark. Marks can be cleared individually or over the
// whole graph (this last op remains O(1)).
//
// The order of edges within a node is not preserved.

// Update functions
// ----------------

// graph:
//  0: nodes, 1: mark count
// nodes:
//  0: prev, 1: next, 2: graph, 3: in, 4: out, 5: data, 6: mark
// edges:
//  0: from, 1: to, 2: data, 3: mark


new_graph = fn " -> graph. Creates a new, empty graph" ()
  vector(false, 1);

copy_graph = fn "graph1 -> graph2. Returns a copy of graph1" (g) 0;

graph_add_node = fn "graph x -> node. Adds a node to graph with value x" (g, x)
  [
    | node |
    node = vector(false, g[0], g, null, null, x, 0);
    if (g[0]) g[0][0] = node;
    g[0] = node;
    node
  ];

graph_remove_node = fn "node -> . Removes node from graph" (node)
  [
    | g |

    g = node[2];
    if (node == g[0]) // removing 1st node
      [
	if (node[1]) // not removing last node
	  [
	    // second node is now first
	    g[0] = node[1];
	    node[1][0] = false;
	  ]
	else
	  g[0] = false // no nodes
      ]
    else
      [
	node[0][1] = node[1];
	if (node[1]) node[1][0] = node[0];
      ]
  ];

graph_add_edge = fn "node1 node2 x -> edge. Adds an edge from node1 to node2, with data x" (n1, n2, x)
  [
    | edge |
    edge = vector(n1, n2, x, 0);
    n1[4] = edge . n1[4];
    n2[3] = edge . n2[3];

    edge
  ];


graph_remove_edge = fn "edge -> . Removes edge from its graph" (e)
  [
    e[0][4] = ldelete!(e, e[0][4]);
    e[1][3] = ldelete!(e, e[1][3]);
  ];

// Accessor operations
// -------------------

graph_nodes = fn "graph -> l. Returns list of nodes of graph" (g)
  [
    | l, dl |
    dl = g[0];

    while (dl)
      [
	l = dl . l;
	dl = dl[1];
      ];
    l
  ];

graph_nodes_apply = fn "fn graph -> l. Applies fn to the of nodes of graph" (f, g)
  [
    | l, dl |
    dl = g[0];

    while (dl)
      [
	f(dl);
	dl = dl[1];
      ];
    l
  ];

graph_node_graph = fn "node -> graph. Returns node's graph" (vector n) n[2];
graph_node_get = fn "node -> x. Returns node's data" (vector n) n[5];
graph_node_set! = fn "node x -> . Sets node's data" (n, x) n[5] = x;


graph_edges_in = fn "node -> l. Returns all ingoing edges of node" (n)
  lcopy(n[3]);

graph_edges_out = fn "node -> l. Returns all outgoing edges of node" (n)
  lcopy(n[4]);

graph_edges_in_apply = fn "fn node -> l. Applies fn to all ingoing edges of node" (f, n)
  lforeach(f, n[3]);

graph_edges_out_apply = fn "fn node -> l. Applies fn to all outgoing edges of node" (f, n)
  lforeach(f, n[4]);

graph_edge_from = fn "edge -> node. Returns node edge is from" (vector e) e[0];
graph_edge_to = fn "edge -> node. Returns node edge is to" (vector e) e[1];
graph_edge_get = fn "edge -> x. Returns edge's data" (vector e) e[2];
graph_edge_set! = fn "edge x -> . Sets edge's data to x" (e, x) e[2] = x;


// Marks
// -----

graph_clear_all_marks = fn "graph -> . Clears all marks on edges and nodes" (g)
  g[1] = g[1] + 1;

graph_mark_node = fn "node -> . Marks node" (n) n[6] = n[2][1];
graph_unmark_node = fn "node -> . Unmarks node" (n) n[6] = 0;
graph_node_marked? = fn "node -> b. True if node marked" (n) n[6] == n[2][1];

graph_mark_edge = fn "edge -> . Marks edge" (e) e[3] = e[0][2][1];
graph_unmark_edge = fn "edge -> . Unmarks edge" (e) e[3] = 0;
graph_edge_marked? = fn "edge -> b. True if edge marked" (e) e[3] == e[0][2][1];
]
