open Emulator

open Arithmetic
open Conversion
open Logical

let () =
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
  print_newline();

  Printf.printf "121 ADD 6 = %d\n" (base2list_to_int add_121_6);
  Printf.printf "121 ADD 10 = %d\n" (base2list_to_int add_121_10);
  Printf.printf "127 ADD -6 = %d\n" (base2list_to_int add_127_minus6);
  Printf.printf "5 SUB 5 = %d\n" (base2list_to_int sub_5_5);
  Printf.printf "5 SUB -5 = %d\n" (base2list_to_int sub_5_minus5);
  print_newline();

  (* Optional *)
  print_endline "Optional:";
  Printf.printf "64 ADD 64 = %d\n" (base2list_to_int add_64_64);
  Printf.printf "10 SUB 11 = %d\n" (base2list_to_int sub_10_11);
  Printf.printf "128 ADD 1 = %d\n" (base2list_to_int add_128_1);