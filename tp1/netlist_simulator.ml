open Netlist_ast

let print_only = ref false
let number_steps = ref (-1)
let rom_addr_size = 2

(* Fast exponentiation *)
let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
      let b = pow a (n / 2) in
      b * b * if n mod 2 = 0 then 1 else a

(** Given an array of booleans representing bytes of an integer in base 2, returns the corresponding integer. *)
let bool_array_to_int array =
  Array.fold_right (fun b i -> (2 * i) + if b then 1 else 0) array 0

(** Convert a value (boolean or boolean array) to an integer*)
let value_to_int = function
| VBit b -> if b then 1 else 0
| VBitArray array -> bool_array_to_int array

(* Pretty printer for the environment *)
let print_environment environment =
  Hashtbl.iter (fun x y -> Printf.printf "%s -> %d\n" x (value_to_int y)) environment

(** 
Simulates the execution of the given program.
The program is assumed to be scheduled.
*)
let simulator program number_steps =
  let number_steps = ref number_steps in
  (* initialises the ROM with zeros as arbitrary values *)
  let rom = Array.make (pow 2 rom_addr_size) false
  (* each equation has its block of RAM *)
  and ram = Hashtbl.create 0 in

  let ram_to_write = Hashtbl.create 0 in

  (* we will store the current values of the variables in a hash table *)
  let context = ref (Hashtbl.create 0) in
  let environment = ref (Hashtbl.create 0) in

  (* look up a variable in the environment and return its value *)
  let find_context_var ident =
    match Hashtbl.find_opt !context ident with
    | None ->
        Printf.printf "Cannot find ident '%s' in environment.\n" ident;
        failwith "Environment error"
    | Some v -> v
  in

  (* utility function to evaluate an argument *)
  let simulate_arg = function
    | Aconst c -> c
    | Avar ident -> find_context_var ident
  in

  (* evaluate the given expression *)
  let simulate_expr eq_ident = function
    | Earg arg -> simulate_arg arg
    | Ereg ident -> (
        try
          Hashtbl.find !environment ident
          (* if the value is not in the environment, then this is the first cycle.
             an arbitrary default value is then returned *)
        with Not_found -> (
          let value = match Env.find ident program.p_vars with
          (* arbitrary default values: zeros everywhere *)
          | TBit -> VBit false
          | TBitArray l -> VBitArray (Array.make l false) in 
          Hashtbl.add !environment ident value ;
          value
        )
      )
    | Enot arg -> (
      match simulate_arg arg with
      | VBit b -> VBit (not b)
      | VBitArray a -> VBitArray (Array.map not a)
    )
    | Ebinop (binop, a1, a2) -> (
        match (simulate_arg a1, simulate_arg a2) with
        | VBit b1, VBit b2 ->
            VBit
              (match binop with
              | Or -> b1 || b2
              | Xor -> (b1 || b2) && not (b1 && b2)
              | And -> b1 && b2
              | Nand -> not (b1 && b2))
        | VBitArray(a1), VBitArray(a2) -> 
          if Array.length a1 <> Array.length a2 
            then failwith "Syntax error: a binary operator can only be applied between two buses of same size" ;
          VBitArray(
            Array.init (Array.length a1) (
              fun i -> let b1 = a1.(i) and b2 = a2.(i) in
              (match binop with
              | Or -> b1 || b2
              | Xor -> (b1 || b2) && not (b1 && b2)
              | And -> b1 && b2
              | Nand -> not (b1 && b2))
            )
          )
        | _, _ -> failwith "Syntax error: a binary operator can only be applied between two bits or two buses")
    | Emux (choice, a1, a2) -> (
        match simulate_arg choice with
        | VBit b -> if b then simulate_arg a2 else simulate_arg a1
        | VBitArray _ -> failwith "MUX: the first argument must be a byte, not a bus"
      )
    | Erom (addr_size, word_size, read_addr) ->
        let read_addr = value_to_int (simulate_arg read_addr) in
        if word_size = 1 then VBit rom.(word_size * read_addr)
        else VBitArray (Array.sub rom read_addr word_size)
    | Eram (addr_size, word_size, read_addr, write_enable, write_addr, data) ->
        (* writing *)
        let write_enable = (match simulate_arg write_enable with
        | VBit b -> b
        | VBitArray _ -> failwith "RAM: write_enable must be a bit, not a bus")
        in
        if write_enable then begin 
          let write_addr = value_to_int (simulate_arg write_addr) in

          (* convert the data value to a boolean array *)
          let data = (match simulate_arg data with 
          | VBit b -> [|b|]
          | VBitArray a -> a
          ) in
          if Array.length data <> word_size then failwith "RAM: data must be of size word_size" ;
          (* wait until the end of the cycle to write to the RAM *)
          Hashtbl.add ram_to_write eq_ident (write_addr, data)
        end ;

        (* reading *)
        let read_addr = value_to_int (simulate_arg read_addr) in
        if not (Hashtbl.mem ram eq_ident) 
          then Hashtbl.add ram eq_ident (Array.make (pow 2 addr_size) false) ;
        let output_array = 
          Array.sub (Hashtbl.find ram eq_ident) read_addr word_size
        in
        (match word_size with
        | 1 -> VBit output_array.(0)
        | _ -> VBitArray output_array)
        
    | Eslice (i1, i2, arg) -> (
        match simulate_arg arg with
        | VBit _ ->
            failwith "SLICE: third argument must be a bus, not a bt"
        | VBitArray array -> VBitArray (Array.sub array i1 (i2 - i1 + 1)))
    | Econcat (arg1, arg2) ->
        let array1 =
          match simulate_arg arg1 with 
          | VBit b -> [| b |] 
          | VBitArray a -> a
        and array2 =
          match simulate_arg arg2 with 
          | VBit b -> [| b |] 
          | VBitArray a -> a
        in
        VBitArray (Array.concat [ array1; array2 ])
    | Eselect (i, a) -> (
        match simulate_arg a with
        | VBit v ->
            if i = 0 then VBit v
            else failwith "SELECT: applied on a byte with non-null index"
        | VBitArray array -> 
          try VBit array.(i) with 
          | Invalid_argument s -> failwith ("SELECT: " ^ s))
  in

  let step = ref 0 in
  while !number_steps <> 0 do
    number_steps := !number_steps - 1;
    incr step;

    Format.printf "Step %d:@." !step ;

    (* asks the user to enter the inputs of the program *)
    List.iter
    (fun ident ->
      Printf.printf "%s ? " ident;

      let input = read_line () in
      Hashtbl.add !context ident
        (if input = "0" then VBit false
        else if input = "1" then VBit true
        else begin
          let l = ref [] in
          for i = 0 to String.length input - 1 do
            l :=
              (match input.[i] with
              | '0' -> false
              | '1' -> true
              | _ -> failwith "Inputs must be given in binary")
              :: !l
          done;
          VBitArray (Array.of_list !l)
        end))
      program.p_inputs;

      (* for each equation, computes the value, and adds it to the context *)
      List.iter
        (fun (ident, expr) -> Hashtbl.add !context ident (simulate_expr ident expr))
        program.p_eqs ;

      (* display the value of each variable *)
      List.iter
        (fun ident ->
          let v = find_context_var ident in
          match v with
          | VBit b -> Format.printf "=> %s = %d\n" ident (if b then 1 else 0)
          | VBitArray a ->
              Format.printf "=> %s = " ident;
              Array.iter (fun b -> Format.printf "%d" (if b then 1 else 0)) a;
              Format.printf "@.")
        program.p_outputs ;

      (* write to the RAM *)
      Hashtbl.iter (fun eq_ident (write_addr, data) -> 
        let ram_block = Hashtbl.find ram eq_ident in
        for i = 0 to (Array.length data) - 1 do 
          ram_block.(write_addr + i) <- data.(i)
        done
      ) ram_to_write ;
      Hashtbl.clear ram_to_write ;

      (* replace the environment by the context of the current cycle *)
      environment := !context ;
      context := Hashtbl.create 0
  done


let compile filename =
  try
    let p = Netlist.read_file filename in
    try
      let p = Scheduler.schedule p in
      if !print_only then 
        let out_name = (Filename.chop_suffix filename ".net") ^ "_sch.net" in
        let out = open_out out_name in
        Netlist_printer.print_program out p
      else simulator p !number_steps
    with Scheduler.Combinational_cycle ->
      Format.eprintf "The netlist has a combinatory cycle.@."
  with Netlist.Parse_error s ->
    Format.eprintf "An error accurred: %s@." s;
    exit 2

let main () =
  Arg.parse
    [ 
      ("-n", Arg.Set_int number_steps, "<n> Number of steps to simulate");
      ("-print_only", Arg.Set print_only, "Print the sorted net-list on standard output without simulating it")
     ]
    compile ""
;;

main ()
