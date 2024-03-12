
open Helper

(* Hexadecimal *)
(* Converts a hexadecimal digit to a base 2 list*)
let hex_digit_to_bin = function
  | '0' -> [0; 0; 0; 0]
  | '1' -> [0; 0; 0; 1]
  | '2' -> [0; 0; 1; 0]
  | '3' -> [0; 0; 1; 1]
  | '4' -> [0; 1; 0; 0]
  | '5' -> [0; 1; 0; 1]
  | '6' -> [0; 1; 1; 0]
  | '7' -> [0; 1; 1; 1]
  | '8' -> [1; 0; 0; 0]
  | '9' -> [1; 0; 0; 1]
  | 'A' | 'a' -> [1; 0; 1; 0]
  | 'B' | 'b' -> [1; 0; 1; 1]
  | 'C' | 'c' -> [1; 1; 0; 0]
  | 'D' | 'd' -> [1; 1; 0; 1]
  | 'E' | 'e' -> [1; 1; 1; 0]
  | 'F' | 'f' -> [1; 1; 1; 1]
  | _ -> invalid_arg "not a hex digit"


(* Removes the "0x" or "0X" prefix from a hexadecimal string if present *)
let remove_hex_prefix hex_str =
  if String.length hex_str >= 2 && (String.sub hex_str 0 2 = "0x" || String.sub hex_str 0 2 = "0X") then
    String.sub hex_str 2 (String.length hex_str - 2)
  else
    hex_str


(* Converts a hexadecimal string to a base 2 list *)
let hex_to_bin hex_str =
  let cleaned_str = remove_hex_prefix hex_str in 
  let rec aux base2list = function
    (* Base case. If hex string is empty then return base2list *)
    | [] -> base2list
    (* Recursive case. h=head, t=tail.
        1. Convert h to binary list
        2. Append result to base2list (@ symbol)
        2. Call aux with tail of hex string *)
    | h :: t -> aux (base2list @ hex_digit_to_bin h) t
  in
  (* Call aux; second parameter is passed implicitly due to function keyword*)
  aux [] (List.of_seq (String.to_seq cleaned_str))


(* Converts a base 2 list to a hexadecimal string *)
let bin_to_hex base2list =
  let rec aux hex_string list = 
    match list with
      | [] -> hex_string (* Base case, return hex string *)
      | h1 :: h2 :: h3 :: h4 :: t -> (* Non-empty list, process 4 bits at a time *)
          let digit = h1 lsl 3 + h2 lsl 2 + h3 lsl 1 + h4 in
          (* %X to convert to hexadecimal. 
            Continue with tail, after first 4 bits *)
          aux (hex_string ^ Printf.sprintf "%X" digit) t
      | _ -> invalid_arg "binary list length must be a multiple of 4"
  in
  aux "" base2list


(* Integer *)
(* Converts an signed integer to a base 2 list *)
let int_to_bin inputInt =
  let rec to_binary n list =
    (* Base case. No more to add, return list *)
    if n = 0 then list
    (* Recursive case. Prepend to list *)
    else to_binary (n / 2) ((n mod 2) :: list)
  in
  let binary = to_binary (abs inputInt) [] in
  (* Ensure it has 8 bits *)
  let rec pad_to_eight list =
    if List.length list = 8 then list
    else pad_to_eight (0 :: list)  (* Prepend zeros until the list is 8 bits long *)
  in
  let padded_binary = pad_to_eight binary in
  (* If the input integer was negative, convert to two's complement *)
  if inputInt >= 0 then padded_binary else twos_complement padded_binary


(* Converts a base 2 list to a signed integer *)
let base2list_to_int base2list =
  match base2list with
  | [] -> failwith "List is empty"
  | h::_ when h = 0 -> (* Positive number, convert directly to int *)
    let rec to_int list resultInt =
      match list with
      | [] -> resultInt
      | hd::tl -> to_int tl (resultInt * 2 + hd)
    in
    to_int (List.tl base2list) 0  (* Ignore the sign bit since the number is positive *)
  | _ -> (* Negative number: two's complement, convert to int, and then negate the int *)
    let positive_list = twos_complement base2list in
    let rec to_int list resultInt =
      match list with
      | [] -> resultInt
      | hd::tl -> to_int tl (resultInt * 2 + hd)
    in
    (* Convert the two's complement list to int and then apply negative sign *)
    -1 * (to_int (List.tl positive_list) 0)  
