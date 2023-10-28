exception Cycle

type mark = NotVisited | InProgress | Visited

type 'a graph = { mutable g_nodes : 'a node list }

and 'a node = {
  n_label : 'a;
  mutable n_mark : mark;
  mutable n_link_to : 'a node list;
  mutable n_linked_by : 'a node list;
}

let mk_graph () = { g_nodes = [] }

let add_node g x =
  let n =
    { n_label = x; n_mark = NotVisited; n_link_to = []; n_linked_by = [] }
  in
  g.g_nodes <- n :: g.g_nodes

let node_of_label g x = List.find (fun n -> n.n_label = x) g.g_nodes

let add_edge g id1 id2 =
  try
    let n1 = node_of_label g id1 in
    let n2 = node_of_label g id2 in
    n1.n_link_to <- n2 :: n1.n_link_to;
    n2.n_linked_by <- n1 :: n2.n_linked_by
  with Not_found ->
    Format.eprintf "Tried to add an edge between non-existing nodes\n";
    raise Not_found

let clear_marks g = List.iter (fun n -> n.n_mark <- NotVisited) g.g_nodes
let find_roots g = List.filter (fun n -> n.n_linked_by = []) g.g_nodes

(** 
Returns true if the given graph is cyclic.
Uses a BFS; if during the search following a node,
the same node is seen twice, then the graph is cyclic.
*)
let has_cycle g =
  let cycle = ref false in
  clear_marks g;

  let rec visit noeud =
    match noeud.n_mark with
    | InProgress -> cycle := true
    | Visited -> ()
    | NotVisited ->
        noeud.n_mark <- InProgress;
        List.iter visit noeud.n_link_to;
        noeud.n_mark <- Visited
  in

  (* we start from each node (in case of multiple connex components) *)
  List.iter visit g.g_nodes;
  !cycle

(** 
Sorts the nodes of the graph in topological order.
This means, if there is a path from u to v, then u
will appear before v in the sorted list of nodes.
If the graph is cyclic, the exception `Cycle` will be raised.
*)
let topological g =
  let sort = ref [] in
  clear_marks g;

  let rec visit noeud =
    match noeud.n_mark with
    | InProgress -> raise Cycle
    | Visited -> ()
    | NotVisited ->
        noeud.n_mark <- InProgress;
        List.iter visit noeud.n_link_to;
        noeud.n_mark <- Visited;
        (* we add the node after visiting the neighbours,
           since the list will be build backwards *)
        sort := noeud.n_label :: !sort
  in

  List.iter visit g.g_nodes;
  !sort
