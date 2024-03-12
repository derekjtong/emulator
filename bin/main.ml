open Emulator
open Printf
open Arithmetic
open Conversion
open Logical

let rec main_menu () =
  printf "Choose an operation:\n";
  printf "1. Bitwise NOT\n";
  printf "2. Bitwise AND\n";
  printf "3. Bitwise OR\n";
  printf "4. Bitwise XOR\n";
  printf "5. Binary Addition\n";
  printf "6. Binary Subtraction\n";
  printf "7. Sample Operations\n";
  printf "8. Exit\n";
  print_string "> ";
  match read_line () with
  | "1" -> ask_for_one_input "Bitwise NOT" bitwise_not
  | "2" -> ask_for_two_inputs "Bitwise AND" bitwise_and
  | "3" -> ask_for_two_inputs "Bitwise OR" bitwise_or
  | "4" -> ask_for_two_inputs "Bitwise XOR" bitwise_xor
  | "5" -> ask_for_two_decimal_inputs "Binary Addition" binary_addition
  | "6" -> ask_for_two_decimal_inputs "Binary Subtraction" binary_subtraction
  | "7" -> sample_operations ()
  | "8" -> printf "Exiting...\n"; exit 0
  | _ -> printf "Invalid option. Please try again.\n\n";
  main_menu ()

and ask_for_one_input operation func  =
  printf "Enter hexadecimal input for %s (e.g., 0xA5): " operation;
  let input = read_line () in
  let result = func (hex_to_bin input) in
  printf "Result: %s\n\n" (bin_to_hex result); (* Display as hexadecimal *)
  main_menu ()

and ask_for_two_inputs operation func  =
  printf "Enter first hexadecimal input for %s (e.g., 0xA5): " operation;
  let input1 = read_line () in
  printf "Enter second hexadecimal input for %s (e.g., 0xA5): " operation;
  let input2 = read_line () in
  let result = func (hex_to_bin input1) (hex_to_bin input2) in
  printf "Result: %s\n\n" (bin_to_hex result); (* Display as hexadecimal *)
  main_menu ()

and ask_for_two_decimal_inputs operation func =
  printf "Enter first decimal input for %s (e.g., 121): " operation;
  let input1 = read_line () |> int_of_string in
  printf "Enter second decimal input for %s (e.g., 10): " operation;
  let input2 = read_line () |> int_of_string in
  let result = func (int_to_bin input1) (int_to_bin input2) in
  printf "Result: %d\n\n" (base2list_to_int result);  (* Display as decimal *)
  main_menu ()

and sample_operations () =
  (* Logical operations *)
  let not_a5 = bitwise_not (hex_to_bin "0xA5") in
  let and_f9_9f = bitwise_and (hex_to_bin "0xF9") (hex_to_bin "0x9F") in
  let or_01_11 = bitwise_or (hex_to_bin "0x01") (hex_to_bin "0x11") in
  let xor_ff_88 = bitwise_xor (hex_to_bin "0xFF") (hex_to_bin "0x88") in

  (* Arithmetic operations *)
  let add_121_6 = binary_addition (int_to_bin 121) (int_to_bin 6) in
  let add_121_10 = binary_addition (int_to_bin 121) (int_to_bin 10) in
  let add_127_minus6 = binary_addition (int_to_bin 127) (int_to_bin (-6)) in
  let sub_5_5 = binary_subtraction (int_to_bin 5) (int_to_bin 5) in
  let sub_5_minus5 = binary_subtraction (int_to_bin 5) (int_to_bin (-5)) in

  (* Optional *)
  let add_64_64 = binary_addition (int_to_bin 64) (int_to_bin 64) in
  let sub_10_11 = binary_subtraction (int_to_bin 10) (int_to_bin 11) in
  let add_128_1 = binary_addition (int_to_bin 128) (int_to_bin 1) in

  (* Printing results *)
  Printf.printf "NOT 0xA5 = %s\n" (bin_to_hex not_a5);
  Printf.printf "0xF9 AND 0x9F = %s\n" (bin_to_hex and_f9_9f);
  Printf.printf "0x01 OR 0x11 = %s\n" (bin_to_hex or_01_11);
  Printf.printf "0xFF XOR 0x88 = %s\n" (bin_to_hex xor_ff_88);
  print_newline ();

  Printf.printf "121 ADD 6 = %d\n" (base2list_to_int add_121_6);
  Printf.printf "121 ADD 10 = %d\n" (base2list_to_int add_121_10);
  Printf.printf "127 ADD -6 = %d\n" (base2list_to_int add_127_minus6);
  Printf.printf "5 SUB 5 = %d\n" (base2list_to_int sub_5_5);
  Printf.printf "5 SUB -5 = %d\n" (base2list_to_int sub_5_minus5);
  print_newline ();

  (* Optional *)
  print_endline "Optional:";
  Printf.printf "64 ADD 64 = %d\n" (base2list_to_int add_64_64);
  Printf.printf "10 SUB 11 = %d\n" (base2list_to_int sub_10_11);
  Printf.printf "128 ADD 1 = %d\n" (base2list_to_int add_128_1);
  print_newline ();
  main_menu ()

let () =
  printf "Welcome to the OCaml Emulator CLI\n";
  main_menu ()