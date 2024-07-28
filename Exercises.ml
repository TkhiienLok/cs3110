(* https://cs3110.github.io/textbook/chapters/basics/exercises.html *)
open Stdlib
let last a = List.nth a (List.length a - 1);;

let rec last2 a = match a with
| [] -> None
| [ x ] -> Some x
| hd :: tl -> last2 tl;;

(* # last_two ["a"; "b"; "c"; "d"];;
- : (string * string) option = Some ("c", "d")
# last_two ["a"];;
- : (string * string) option = None *)

let rec last_two = function
  | [] | [_] -> None
  | [x1; x2] -> Some (x1, x2)
  | _ :: t -> last_two t;;

let rec length = function
  | [] -> 0
  | _ :: t -> 1 + length t;;

let rec reverse = function
  | [] -> []
  | h :: t ->  (reverse t) @ [h]

let rev list =
    let rec aux acc = function
      | [] -> acc
      | h :: t -> aux (h :: acc) t
    in
    aux [] list;;

let compare_chars c1 c2 =
  Char.equal c1 c2;;


(** [is_palindrome lst] is true when [lst] is polindrom. *)
let reverse_half lst = 
  let half_len = (List.length lst ) / 2 in
    let rec aux acc = function
    | [] -> acc
    | h :: t -> if List.length acc < half_len then aux (h :: acc) t else acc

  in
  aux [] lst;; 
  
let split_list_in_half lst = (* Execution time: 84.607s with @ operator on 102912 elements and 44.377s with :: operator, Execution time: 0.00276s 🎉 without applying List.length on any of recursion step *)
  let length = List.length lst in
  let half_len = length / 2 in
  let rec split_list left_list right_list list_to_split_length = function
    | [] -> left_list, right_list
    | hd :: tl ->
      let tail_length = list_to_split_length - 1 in 
        if (length - tail_length) <= half_len
          then split_list (hd :: left_list) tl tail_length tl
        else if (list_to_split_length > half_len) then left_list, tl
        else left_list, hd :: tl

    in  split_list [] [] length lst ;;

let is_chars_palindrome lst =  (* Execution time: 44.616s on list of 102912 char elements, e.g. n_list_of_chars multiplied by 400, but when tail recursive 0.00323s 🎉 *)
  let left_list_reversed, right_list = split_list_in_half lst in
  List.equal compare_chars left_list_reversed right_list
;;
let is_ints_palindrome lst =
  let left_list_reversed, right_list = split_list_in_half lst in
  List.equal (fun x1 x2 -> x1 = x2) left_list_reversed (rev right_list)
;;


(* Define a function that computes the sign (1, 0, or -1) of an integer. Use a nested if expression. Test your function by applying it to a few inputs.*)
(* https://cs3110.github.io/textbook/chapters/basics/exercises.html# *)
  let  number_sign x = match (x / (abs x)) with
| 1 -> 1
| -1 -> -1
|  exception Division_by_zero | _ -> 0

(* Define a function that computes the area of a circle given its radius. Test your function with assert. *)
let circle_area r = (r **. 2.) *. 3.14;;

(* Define a function that computes the root mean square of two numbers—i.e. sqrt{(x^2 + y^2) / 2} . Test your function with assert. *)

let root_mean_square x y = ((x **. 2. +. y **. 2.) /. 2.) **. 0.5;;

(* Define a function that takes an integer d and string m as input and returns true just when d and m form a valid date. Here, a valid date has a month that is one of the following abbreviations: Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sept, Oct, Nov, Dec. And the day must be a number that is between 1 and the minimum number of days in that month, inclusive. For example, if the month is Jan, then the day is between 1 and 31, inclusive, whereas if the month is Feb, then the day is between 1 and 28, inclusive. *)
let date d m =
  match m with 
  | "Jan" | "Mar" | "May"  | "Jul" | "Aug" | "Oct" | "Dec"  -> d >= 1 && d <= 31 
  | "Apr" | "Jun" |  "Sept" |  "Nov" -> d >= 1 && d <= 30 
  | "Feb"  -> d >= 1 && d <= 29 
  | _ ->  false;;

(* the function stops working if not opening Stdlib explicitly case istalled base and core libraries shaddow Stdlib operators *)
let time_function f x =
  let start_time = Sys.time () in
  let result = f x in
  let end_time = Sys.time () in
  Printf.printf "Execution time: %.5fs\n" (end_time -. start_time);
  result

(* Define a recursive function fib : int -> int, such that fib n is the nth number in the Fibonacci sequence, which is 1, 1, 2, 3, 5, 8, 13, … That is: *)

let rec fib = function
  | 1 | 2 -> 1
  | n -> fib (n-1) + fib (n-2)

let fib_fast n = 
  let rec h n pp p = if n = 1 then p else h (n-1) (p) (pp + p)
  in h n 0 1;;
  
(* Write a function divide : numerator:float -> denominator:float -> float. Apply your function. *)

let devide (numerator:float) (denominator:float) =
  if denominator = 0. then failwith "Denominator can not be 0" else numerator /. denominator;;
(* ----------------------------------------------------------- *)
(* Data and types *)

(* Exercise: product [★★]

Write a function product that returns the product of all the elements in a list. The product of all the elements of an empty list is 1 *)
let rec product = function
  | [] -> 1
  | h :: t -> h * product t;;

(* Exercise: concat [★★]
Write a function that concatenates all the strings in a list. The concatenation of all the strings in an empty list is the empty string "". *)
let rec concat = function 
  | [] -> ""
  | h :: t -> h ^ concat t;;

(* Exercise: product test [★★]

Unit test the function product that you wrote in an exercise above. *)
(* the dune project with ounit2 tests is in ./numberListProductWithUnitTests
  Read the dune guide https://dune.readthedocs.io/en/stable/howto/install-dune.html after dune setup run in terminal:
  
   cd ./numberListProductWithUnitTests
   dune build
   dune exec ./test_number_list_product.exe
*)

(* Exercise: patterns [★★★]
  Using pattern matching, write three functions, one for each of the following properties. 
  Your functions should return true if the input list has the property and false otherwise.
    - the list’s first element is "bigred"
    - the list has exactly two or four elements; do not use the length function
    - the first two elements of the list are equal
*)
let first_bigred = function
  | [] -> false
  | h :: _ -> h = "bigred";;

let length_two_or_four = function
  | [_; _] | [_; _; _; _]-> true
  | _ -> false;;

let two_first_are_equal = function
  | [] -> false
  | h :: t -> begin 
    match t with
      | [] -> false
      | h' :: t' -> h = h'
  end
    
let eq_first_two = function
  | a :: b :: _ -> a = b
  | _ -> false


(* Exercise: library [★★★]

Consult the List standard library to solve these exercises:

Write a function that takes an int list and returns the fifth element of that list, if such an element exists. If the list has fewer than five elements, return 0. Hint: List.length and List.nth.

Write a function that takes an int list and returns the list sorted in descending order. Hint: List.sort with Stdlib.compare as its first argument, and List.rev. *)

let compare_int x y =
  if x < y then -1
  else if x > y then 1
  else 0;;

let numbers = [4; 2; 7; 1; 9];;

let sorted_numbers = List.sort compare_int numbers;;

let fifth_element lst =
  if List.length lst >= 5 then List.nth lst (4) else 0;;

let sorted_discended lst = 
  List.rev (List.sort Stdlib.compare lst);;

(* Exercise: library puzzle [★★★] 26.07.2024 *) 

(** [last_el lst] is the last element of a [lst]. The [lst] must be non-empty. *)
let last_el lst = 
  if (List.length lst) < 0 then invalid_arg "The list must be not empty";
  List.nth lst (List.length lst - 1);;


(** [any_zeros lst] is true if and only if [lst] contains at least one 0. *)

let  any_zeros_v1 lst = 
  try
    match (List.find (fun el -> el = 0) lst) with 
      | _ -> true
  with
  | Not_found -> false;;

let  any_zeros_v2 lst = List.exists (fun el -> el = 0) lst;;


(* Generating list of numbers till n *)

let rec unfold_right f init =
  match f init with
  | None -> []
  | Some (x, next) -> x :: unfold_right f next

(** [range n] is a generated list of natural numbers of length [n] starting from 1 *)
let range n =
  let irange x = if x > n then None else Some (x, x + 1) in
  unfold_right irange 1

(** [n_list_of_chars n] is generated list of chars FOR TESTING PURPOSES *)
let all_chars =
  let rec aux i acc =
    if i < 0 then acc else aux (i - 1) (Char.chr i::acc)
  in aux 255 []

(** [n_list_of_chars n] is generated list of chars multiplied by [n] - FOR TESTING PURPOSES *)
let n_list_of_chars n = 
 let rec aux i acc =
  if i < 0 then acc else aux (i -1 ) (all_chars @ acc)

in aux n [] 
(* Exercise: take drop [★★★] *)

let take n lst = (* if bad version with calling List.length on each step - Execution time: 27.945s on list of 102912 elenments, but if tail recursive Execution time: 0.006s 🎉 on 102911 elements*)
  let lst_length = List.length lst in 
  if lst_length <= n then lst else 
    let rec take_n_of_lst lst' lst_length' =
      match lst' with 
      | [] -> []
      | hd :: tl -> let tail_length' = lst_length' - 1 in 
        if tail_length' >= lst_length - n 
                      then hd :: (take_n_of_lst tl tail_length') 
                      else take_n_of_lst tl tail_length'
    in take_n_of_lst lst lst_length;;