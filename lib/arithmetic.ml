open Helper

(* All operations are 8 bit signed integers, -128 to 127 *)

(* ADD operation on 8-bit signed integer base 2 list *)
let binary_addition b1 b2 =
  let rec add_lists l1 l2 carry list =
    match (l1, l2) with
      (* Base case, carry is ignored *)
    | ([], []) ->  list
      (* Non-empty list, destructure head and tail *)
    | (h1::t1, h2::t2) ->
      let sum = h1 + h2 + (if carry then 1 else 0) in
      let new_carry = sum >= 2 in
      let result_bit = sum mod 2 in
      (* Prepend result_bit *)
      add_lists t1 t2 new_carry (result_bit::list)
      (* Should not occur, but just in case *)
    | (_, _) -> failwith "Lists are not the same length" 
  in
  add_lists (List.rev b1) (List.rev b2) false [] 

(* SUB operation on 8-bit signed integer base 2 list *)
let binary_subtraction b1 b2 =
  (* Reuse addition *)
  binary_addition b1 (twos_complement b2)
