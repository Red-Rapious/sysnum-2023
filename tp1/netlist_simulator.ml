open Netlist_ast

let print_only = ref false
let number_steps = ref (-1)

(** 
Simulates the execution of the given program.
The program is assumed to be scheduled.
*)
let simulator program number_steps = 
  let number_steps = if number_steps >= 1 then number_steps else 1 in
  (*let rom = Array.make 10_000 false in*)

  (* we will store the current values of the variables in a hash table *)
  let environment = Hashtbl.create (List.length program.p_outputs) in

  (* asks the user to enter the inputs of the program *)
  List.iter (fun ident ->
    Printf.printf "%s ? " ident ;
    let input = read_line () in
    Hashtbl.add environment ident (
      if input = "0" then VBit(false) 
      else if input = "1" then VBit(true) 
      else 
        (*let i = ref i in
        let l = ref [] in 
        while !i > 0 do 
          let byte = !i mod 10 in
          if (byte != 0 && byte != 1) then failwith "inputs must be given in binary";
          l := (if byte = 0 then false else true) :: !l ;
          i := !i / 10
        done ;
        VBitArray(Array.of_list !l)*)
        let l = ref [] in
        for i = 0 to (String.length input) - 1 do
          l := (
            match input.[i] with 
            | '0' -> false
            | '1' -> true
            | _ -> failwith "inputs must be given in binary"
          )
          :: !l
        done ;
        VBitArray(Array.of_list !l)
    )
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
    | VBitArray(_) -> failwith "Syntax error: the first argument of MUX must be a byte, not a bus"
    end
  | Erom(addr_size, word_size, read_addr) -> failwith "VBitArrays are not implemented yet"
    (*if word_size = 1 then rom.(word_size * (simulate_arg read_addr)) else failwith "ROM only available for VBit"*)
  | Eram(addr_size, word_size, read_addr, write_enable, write_addr, data) -> failwith "VBitArrays are not implemented yet"
  | Eslice(i1, i2, arg) -> 
    (
      match simulate_arg arg with 
      | VBit(_) -> failwith "Syntax error: SLICE must be called on a bus, not a byte"
      | VBitArray(array) -> VBitArray(Array.sub array i1 (i2-i1+1))
    )
  | Econcat(arg1, arg2) -> 
    let array1 = 
      match simulate_arg arg1 with
      | VBit(b) -> [|b|]
      | VBitArray(a) -> a
    and array2 = 
      match simulate_arg arg2 with
      | VBit(b) -> [|b|]
      | VBitArray(a) -> a
    in 
    VBitArray(Array.concat [array1; array2])
  | Eselect(i, a) -> 
    match simulate_arg a with
    | VBit(v) -> if i = 0 then VBit(v) else failwith "SELECT applied with non-null index on a byte"
    | VBitArray(array) -> VBit(array.(i)) (* TODO: catch array out of range *)
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
      let v = find_environment_var ident in 
      match v with
        | VBit(b) -> Format.printf "=> %s = %d\n" ident (if b then 1 else 0)
        | VBitArray(a) -> 
          Format.printf "=> %s = " ident ;
          Array.iter (
            fun b -> Format.printf "%d" (if b then 1 else 0)
          ) a ;
          Format.printf "\n"
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