(* All operations are 8 bit unsigned integers, 0x0 to 0xFF *)

(* Bit-wise AND operation on two base 2 lists *)
let rec bitwise_and l1 l2 =
  match (l1, l2) with
  (* Non-empty list. Prepend min head to recursive result *)
  | (h1 :: t1, h2 :: t2) -> (min h1 h2) :: bitwise_and t1 t2
  (* Empty list, return empty list *)
  | _ -> [];;

(* Bit-wise OR operation on two base 2 lists *)
let rec bitwise_or l1 l2 =
  match (l1, l2) with
  (* Non-empty list. Prepend max head to recursive result *)
  | (h1 :: t1, h2 :: t2) -> (max h1 h2) :: bitwise_or t1 t2
  (* Empty list, return empty list *)
  | _ -> [];;

(* Bit-wise XOR operation on two base 2 lists *)
let rec bitwise_xor l1 l2 =
  match (l1, l2) with
  (* Non-empty list. Prepend xor h1 h2 to recursive result *)
  | (h1 :: t1, h2 :: t2) ->
    let xor_result = if h1 = h2 then 0 else 1 in
    xor_result :: bitwise_xor t1 t2
  | _ -> [];;

(* Bit-wise NOT operation on a base 2 list *)
let bitwise_not l =
  List.map (fun b -> if b = 0 then 1 else 0) l;;