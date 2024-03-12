open OUnit2
open Emulator
open Arithmetic
open Conversion
open Logical

let test_logical_operations _ =
  let not_a5 = bitwise_not (hex_to_bin "0xA5") in
  let and_f9_9f = bitwise_and (hex_to_bin "0xF9") (hex_to_bin "0x9F") in
  let or_01_11 = bitwise_or (hex_to_bin "0x01") (hex_to_bin "0x11") in
  let xor_ff_88 = bitwise_xor (hex_to_bin "0xFF") (hex_to_bin "0x88") in

  assert_equal ~printer:(fun x -> x) "5A" (bin_to_hex not_a5);
  assert_equal ~printer:(fun x -> x) "99" (bin_to_hex and_f9_9f);
  assert_equal ~printer:(fun x -> x) "11" (bin_to_hex or_01_11);
  assert_equal ~printer:(fun x -> x) "77" (bin_to_hex xor_ff_88)

let test_arithmetic_operations _ =
  let add_121_6 = binary_addition (int_to_bin 121) (int_to_bin 6) in
  let add_121_10 = binary_addition (int_to_bin 121) (int_to_bin 10) in
  let add_127_minus6 = binary_addition (int_to_bin 127) (int_to_bin (-6)) in
  let sub_5_5 = binary_subtraction (int_to_bin 5) (int_to_bin 5) in
  let sub_5_minus5 = binary_subtraction (int_to_bin 5) (int_to_bin (-5)) in
  let add_64_64 = binary_addition (int_to_bin 64) (int_to_bin 64) in
  let sub_10_11 = binary_subtraction (int_to_bin 10) (int_to_bin 11) in
  let add_128_1 = binary_addition (int_to_bin 128) (int_to_bin 1) in

  assert_equal ~printer:string_of_int 127 (base2list_to_int add_121_6);
  assert_equal ~printer:string_of_int (-125) (base2list_to_int add_121_10);
  assert_equal ~printer:string_of_int 121 (base2list_to_int add_127_minus6);
  assert_equal ~printer:string_of_int 0 (base2list_to_int sub_5_5);
  assert_equal ~printer:string_of_int 10 (base2list_to_int sub_5_minus5);
  assert_equal ~printer:string_of_int 0 (base2list_to_int add_64_64);
  assert_equal ~printer:string_of_int (-1) (base2list_to_int sub_10_11);
  assert_equal ~printer:string_of_int (-127) (base2list_to_int add_128_1)

let suite =
  "EmulatorTestSuite" >::: [
    "test_logical_operations" >:: test_logical_operations;
    "test_arithmetic_operations" >:: test_arithmetic_operations;
  ]

let () =
  run_test_tt_main suite
