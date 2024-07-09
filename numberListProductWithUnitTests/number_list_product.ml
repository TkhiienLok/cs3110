(* Exercise: product [★★]

Write a function product that returns the product of all the elements in a list. The product of all the elements of an empty list is 1 *)

let rec number_list_product = function
  | [] -> 1
  | h :: t -> h * number_list_product t