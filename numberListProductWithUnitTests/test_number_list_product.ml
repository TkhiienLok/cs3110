
open OUnit2
open Number_list_product

let make_number_list_product_test name expected_output input =
  name >:: (fun _ -> assert_equal expected_output (number_list_product input) ~printer:string_of_int)

let tests = "test suite for number_list_product" >::: [
  make_number_list_product_test "empty" 1 [];
  make_number_list_product_test "singleton" 2  [2];
  make_number_list_product_test "two_elements" 6  [2; 3];
  make_number_list_product_test "with_non_even_number_of_negative" (-8)  [2; 4; -1];
  make_number_list_product_test "with_even_number_of_negative" 10  [-5; -2];
]

let _ = run_test_tt_main tests