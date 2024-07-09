(* https://cs3110.github.io/textbook/chapters/basics/exercises.html *)

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


let is_palindrome lst = lst = rev lst;;


(* Define a function that computes the sign (1, 0, or -1) of an integer. Use a nested if expression. Test your function by applying it to a few inputs.*)
(* https://cs3110.github.io/textbook/chapters/basics/exercises.html# *)
  let  number_sign x = match (x / (abs x)) with
| 1 -> 1
| -1 -> -1
|  exception Division_by_zero | _ -> 0

(* Define a function that computes the area of a circle given its radius. Test your function with assert. *)

let circle_area r = (r ** 2.) *. 3.14;;

(* Define a function that computes the root mean square of two numbers—i.e. sqrt{(x^2 + y^2) / 2} . Test your function with assert. *)

let root_mean_square x y = ((x ** 2. +. y ** 2.) /. 2.) ** 0.5;;

(* Define a function that takes an integer d and string m as input and returns true just when d and m form a valid date. Here, a valid date has a month that is one of the following abbreviations: Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sept, Oct, Nov, Dec. And the day must be a number that is between 1 and the minimum number of days in that month, inclusive. For example, if the month is Jan, then the day is between 1 and 31, inclusive, whereas if the month is Feb, then the day is between 1 and 28, inclusive. *)
let date d m =
  match m with 
  | "Jan" | "Mar" | "May"  | "Jul" | "Aug" | "Oct" | "Dec"  -> d >= 1 && d <= 31 
  | "Apr" | "Jun" |  "Sept" |  "Nov" -> d >= 1 && d <= 30 
  | "Feb"  -> d >= 1 && d <= 29 
  | _ ->  false;;


let time_function f x =
  let start_time = Sys.time () in
  let result = f x in
  let end_time = Sys.time () in
  Printf.printf "Execution time: %.3fs\n" (end_time -. start_time);
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
  List.rev (List.sort Stdlib.compare lst); 