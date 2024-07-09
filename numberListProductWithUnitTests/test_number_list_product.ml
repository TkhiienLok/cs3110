
open OUnit2
open Number_list_product

let tests = "test suite for number_list_product" >::: [
  "empty" >:: (fun _ -> assert_equal 1 (number_list_product []));
  "singleton" >:: (fun _ -> assert_equal 2 (number_list_product [2]));
  "two_elements" >:: (fun _ -> assert_equal 2 (number_list_product [1; 2]));
  "with_non_even_number_of_negative" >:: (fun _ -> assert_equal (-6) (number_list_product [2; 3; -1]));
  "with_even_number_of_negative" >:: (fun _ -> assert_equal (10) (number_list_product [-5; -2;]));
]

let _ = run_test_tt_main tests