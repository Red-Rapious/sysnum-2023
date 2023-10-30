open Netlist_ast
open Graph

exception Combinational_cycle

let read_exp (id, ex) =
  let list = ref [ id ] in

  let read_arg a =
    match a with Avar ident -> list := ident :: !list | Aconst _ -> ()
  in

  (match ex with
  | Earg a -> read_arg a
  | Ereg _ -> ()
  | Enot a -> read_arg a
  | Ebinop (op, a1, a2) ->
      read_arg a1;
      read_arg a2
  | Emux (a1, a2, a3) ->
      read_arg a1;
      read_arg a2;
      read_arg a3
  | Erom (_, _, a) -> read_arg a
  | Eram (_, _, a, _, _, _) -> read_arg a (* only read_addr is used to compute the value *)
  | Econcat (a1, a2) ->
      read_arg a1;
      read_arg a2
  | Eslice (_, _i2, a) -> read_arg a
  | Eselect (_, a) -> read_arg a);

  !list

let schedule p =
  let g = mk_graph () in

  (* ajout des noeuds, qui sont les variables *)
  List.iter (fun i -> add_node g i) p.p_inputs;
  List.iter (fun (i, _) -> add_node g i) p.p_eqs;

  (* ajout des arêtes *)
  List.iter
    (fun (output, eq) ->
      List.iter
        (fun input -> if input != output then add_edge g input output)
        (read_exp (output, eq)))
    p.p_eqs;

  (* filtrage de la liste pour ne garder que les variables correspondant à une équation *)
  let sorted_eq = topological g in
  let variables_with_eq = Hashtbl.create 0 in
  List.iter (fun eq -> Hashtbl.add variables_with_eq (fst eq) eq) p.p_eqs;
  let filtered_list =
    List.filter (fun ident -> Hashtbl.mem variables_with_eq ident) sorted_eq
  in
  try
    {
      p_eqs =
        (* correspondance idents <-> equations *)
        List.map
          (fun ident -> Hashtbl.find variables_with_eq ident)
          filtered_list;
      p_inputs = p.p_inputs;
      p_outputs = p.p_outputs;
      p_vars = p.p_vars;
    }
  with Cycle -> raise Combinational_cycle
