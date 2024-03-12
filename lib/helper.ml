(* Negate a binary number using two's complement *)
let twos_complement list =
  (* Invert list *)
  let inverted = List.map (fun bit -> if bit = 0 then 1 else 0) list in
  (* Define add*)
  let rec add_one list carry =
    match list with
      (* Base case, empty list*)
    | [] -> if carry then [1] else []
      (* Non-empty list, destructure head and tail*)
    | h::t -> if carry then
      (* Head is 0, carry won't cause carry-over, put 1 *)
      if h = 0 then 1::(add_one t false)
      (* Head is 1, carry causes carry, put 0 *)
      else 0::(add_one t true)
      (* No carry, continue *)
      else h::(add_one t false)
  (* Call above *)
  in add_one (List.rev inverted) true |> List.rev


(* Print binary list *)
let print_binary_list base2list =
  List.iter (Printf.printf "%d") base2list;
  print_newline ()