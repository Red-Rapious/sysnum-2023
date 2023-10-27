open Netlist_ast

let print_only = ref false
let number_steps = ref (-1)

(** 
Simulates the execution of the given program.
The program is assumed to be scheduled.
*)
let simulator program number_steps = 
  if number_steps <= 0 then failwith "Warning: the number of steps is null" ;
  (*let rom = Array.make 10_000 false in*)

  (* we will store the current values of the variables in a hash table *)
  let environment = Hashtbl.create (List.length program.p_outputs) in

  (* asks the user to enter the inputs of the program *)
  List.iter (fun ident ->
    Printf.printf "%s ? " ident ;
    let i = read_int () in
    Hashtbl.add environment ident (if i = 0 then VBit(false) else VBit(true)) (* TODO: VBitArray *)
  ) program.p_inputs ;

  (* TODO: add default values to environment for REG *)

  (* look up a variable in the environment and return its value *)
  let find_environment_var ident = 
    match Hashtbl.find_opt environment ident with
  | None -> Printf.printf "Cannot find ident '%s' in environment.\n" ident ; failwith "Environment error"
  | Some(v) -> v
  in

  (* utility function to return the value of an argument *)
  let simulate_arg = function
  | Aconst(c) -> c
  | Avar(ident) -> find_environment_var ident
  in

  (* return the value of the given expression *)
  let simulate_expr = function
  | Earg(arg) -> simulate_arg arg
  | Ereg(ident) -> find_environment_var ident
  | Enot(arg) -> simulate_arg arg
  | Ebinop(binop, a1, a2) -> 
      (match (simulate_arg a1, simulate_arg a2) with
      | VBit(b1), VBit(b2) -> VBit(
        match binop with 
        | Or -> b1 || b2
        | Xor -> (b1 || b1) && not (b1 && b2)
        | And -> b1 && b2
        | Nand -> not (b1 && b2)
      )
      | _, _ -> failwith "VBitArrays are not implemented yet")
  | Emux(choice, a1, a2) -> begin
    match simulate_arg choice with
    | VBit(b) -> if b then simulate_arg a2 else simulate_arg a1
    | VBitArray(_) -> failwith "VBitArrays are not implemented yet"
    end
  | Erom(addr_size, word_size, read_addr) -> failwith "VBitArrays are not implemented yet"
    (*if word_size = 1 then rom.(word_size * (simulate_arg read_addr)) else failwith "ROM only available for VBit"*)
  | Eram(addr_size, word_size, read_addr, write_enable, write_addr, data) -> failwith "VBitArrays are not implemented yet"
  | Econcat(_) | Eslice(_) | Eselect(_) -> failwith "VBitArrays are not implemented yet"
  in
  
  (* for each equation, compute the value and add it to the environment *)
  for i = 0 to number_steps - 1 do
    List.iter (fun (ident, expr) -> Hashtbl.add environment ident (simulate_expr expr)) program.p_eqs
  done ;

  (*Printf.printf "keys: " ;
  Seq.iter (fun ident -> Printf.printf "%s, " ident) (Hashtbl.to_seq_keys environment); *)

  (* display the value of each variable *)
  List.iter (
    fun ident -> 
      Format.printf "=> %s = %d\n" ident (
      let v = find_environment_var ident in 
      match v with
        | VBit(b) -> if b then 1 else 0
        | _ -> failwith "VBitArrays are not implemented yet")
    ) program.p_outputs


let compile filename =
  try
    let p = Netlist.read_file filename in
    begin try
        let p = Scheduler.schedule p in
        simulator p !number_steps
      with
        | Scheduler.Combinational_cycle ->
            Format.eprintf "The netlist has a combinatory cycle.@.";
    end;
  with
    | Netlist.Parse_error s -> Format.eprintf "An error accurred: %s@." s; exit 2

let main () =
  Arg.parse
    ["-n", Arg.Set_int number_steps, "Number of steps to simulate"]
    compile
    ""
;;

main ()