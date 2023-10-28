open Netlist_ast
let print_only = ref false
let number_steps = ref (-1)

let simulator program n_steps rom ram =
    let ctx = ref Env.empty and env = ref Env.empty and step_number = ref 0 and ram = ref ram and ramwrite = ref [] in
    number_steps := n_steps;

    while !number_steps <> 0 do
        number_steps := !number_steps - 1;
        step_number := !step_number + 1;
        Format.printf "§ Step %d\n@?" !step_number;

        let read_bit id =
            let rec retry () =
              try
                Format.printf "%s ? @?" id;
                match read_int () with
                | 0 -> false
                | 1 -> true
                | _ -> raise Exit
              with _ -> (print_endline "Invalid bit"; retry ())
            in retry ()
        in
        ctx := List.fold_left 
        (fun ctx id -> 
            Env.add id (match Env.find id program.p_vars with
                | TBit -> VBit(read_bit id)
                | TBitArray(l) -> VBitArray(Array.init l (fun i -> read_bit (id^"["^(string_of_int i)^"]")))
            ) ctx
        ) !ctx program.p_inputs;

        let value ctx arg = match arg with
            | Avar(id) -> Env.find id ctx
            | Aconst(v) -> v
        in let bit ctx arg = match value ctx arg with
            | VBit(b) -> b
            | _ -> assert false
        in let bitarray ctx arg = match value ctx arg with
            | VBitArray(a) -> a
            | _ -> assert false
        in let rec int_of_bitarray v = match v with
            | VBit(b) -> int_of_bitarray (VBitArray([|b|]))
            | VBitArray(a) -> Array.fold_right (fun b n -> 2*n + (if b then 1 else 0)) a 0
        in
        env := !ctx;

        ramwrite := [];
        List.iter (fun (id, exp) ->
            env := Env.add id (match exp with
                | Earg(arg) -> value !env arg
                | Ereg(id) ->
                    (try value !ctx (Avar(id)) with | Not_found -> (match Env.find id program.p_vars with
                        | TBit -> VBit(false)
                        | TBitArray(l) -> VBitArray(Array.make l false)
                    ))
                | Enot(arg) -> VBit (not(bit !env arg))
                | Ebinop(op, arg1, arg2) ->
                    let f b1 b2 = (match op with
                        | Or -> b1 || b2
                        | Xor -> b1 <> b2
                        | And -> b1 && b2
                        | Nand -> not(b1 && b2)
                    ) in
                    let map2 f a1 a2 =
                        let a3 = Array.make (Array.length a1) false in
                        for i = 0 to (Array.length a1) - 1 do
                            a3.(i) <- f a1.(i) a2.(i)
                        done;
                        a3
                    in
                    (match (value !env arg1, value !env arg2) with
                        | (VBit(b1), VBit(b2)) -> VBit(f b1 b2)
                        | (VBitArray(a1), VBitArray(a2)) -> VBitArray(map2 f a1 a2)
                        | _ -> assert false)
                | Emux(cond, arg1, arg2) -> if bit !env cond then value !env arg1 else value !env arg2
                | Erom(addr_size, word_size, read_addr) -> 
                    let read_index = word_size * (int_of_bitarray (value !env read_addr)) in
                    if word_size = 1 then VBit(rom.(read_index)) else VBitArray(Array.sub rom read_index word_size)
                | Eram(addr_size, word_size, read_addr, write_enable, write_addr, data) ->
                    let read_index = word_size * (int_of_bitarray (value !env read_addr)) in
                    if read_index + word_size > (Array.length !ram) then (
                        ram := Array.init (read_index+word_size) (fun i -> try !ram.(i) with | Invalid_argument(s) -> false)
                    );
                    if bit !env write_enable then ramwrite := exp :: !ramwrite;
                    if word_size = 1 then VBit(!ram.(read_index)) else VBitArray(Array.sub !ram read_index word_size)
                | Econcat(arg1, arg2) -> (match (value !env arg1, value !env arg2) with
                    | (VBitArray(a1), VBitArray(a2)) -> VBitArray(Array.concat [a1; a2])
                    | (VBitArray(a1), VBit(b2)) -> VBitArray(Array.concat [a1; [|b2|]])
                    | (VBit(b1), VBitArray(a2)) -> VBitArray(Array.concat [[|b1|]; a2])
                    | (VBit(b1), VBit(b2)) -> VBitArray([|b1; b2|]))
                | Eslice(i1, i2, arg) -> VBitArray(Array.sub (bitarray !env arg) i1 (i2 - i1 + 1))
                | Eselect(i, arg) -> match value !env arg with | VBit(b) -> VBit(b) | VBitArray(a) -> VBit(a.(i))
            ) !env
        ) program.p_eqs;

        ctx := !env;

        List.iter (function
            | Eram(addr_size, word_size, read_addr, write_enable, write_addr, data) ->
                if bit !ctx write_enable then (
                    let write_index = word_size * (int_of_bitarray (value !ctx write_addr)) in
                    if word_size = 1 then
                        !ram.(write_index) <- (try bit !ctx data with | Not_found -> false)
                    else
                        Array.iteri (fun i b -> !ram.(write_index + i) <- b) (try bitarray !ctx data with | Not_found -> Array.make word_size false)
                )
            | _ -> assert false
        ) !ramwrite;

        (* Write the outputs*)

        let string_of_bool b = if b then "1" else "0" in
        let print_value id =
            Format.printf "%s : %s\n@?" id (match Env.find id !ctx with
                | VBit(b) -> string_of_bool(b)
                | VBitArray(a) ->
                    snd(Array.fold_left (fun (i, s) b ->
                        (i+1, (string_of_bool b)^s)
                    ) (0, "") a)
            )
        in
        List.iter (fun id -> print_value id) program.p_outputs
    done

        
let compile filename =
            try
              let p = Netlist.read_file filename in
              begin try
                  let p = Scheduler.schedule p in
                  simulator p !number_steps [||] [||]
                with
                  | Scheduler.Combinational_cycle ->
                      Format.eprintf "The netlist has a combinatory cycle.@." ;
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