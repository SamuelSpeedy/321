open Funs

(* extra helper functions for everything that allows helper functions *)
(* a helper function that returns the first element of a list *)
let rec first_element lst = match lst with
| [x] -> x
| h :: t -> h;;

(* a helper function that obtains everything in the list except the first element *)
let rec list_without_first_element lst = match lst with
| [] -> []
| h :: t -> t;;

(* a helper function that returns the final element of a list *)
let rec last_element lst = match lst with
| [x] -> x
| h :: t -> last_element t;;

(* Recursive Helper Function to find the length of a list *)
let rec length_of_list lst = match lst with
    [] -> 0
  | h :: t -> 1 + length_of_list t;;

(***********************************)
(* Part 1: Non-Recursive Functions *)
(* Rules
  1. You can use any helper functions and they can be recursive
  2. You may not use any imperative features such as loops and ref 
  3. You may only use library functions in Stdlib *)
(***********************************)

(* Points to Make:
   1. x is an integer
   Type of the Function: int -> int *)
let abs x = 
  if x = 0 
    then 0 
  else
    if x > 0 
      then x 
    else 
      -1 * x;;

(* Points to Make:
  1. tup is a tuple of type ('a * 'b * 'c)
  Type of the Function: ('a * 'b * 'c) -> ('c * 'b * 'a) *)
let rev_tup tup = 
  match tup with
  (x, y, z) -> (z, y, x);;

(* Points to Make:
  1. x is of type int
  Type of the Function: int -> bool *)
let is_even x = 
  if x mod 2 = 0
    then true
  else
    false;;

(* Points to Make:
  1. both point1 and point2 are of type (int * int)
  Type of the Function: (int * int) -> (int * int) -> int *)
let area point1 point2 = 
  let x1 = match point1 with 
    (x, y) -> x in
  let x2 = match point2 with 
    (x, y) -> x in
  let y1 = match point1 with 
    (x, y) -> x in
  let y2 = match point2 with 
    (x, y) -> x in
  (abs (x2 - x1)) * (abs (y2 - y1));;

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

(* Points to Make:
  1. n is of type int
  Type of the Function: int -> int *)
let rec fibonacci n =
  if (n = 0) then
    0
  else 
    if (n = 1 || n = 2) then
      1
    else
      fibonacci(n - 1) + fibonacci(n - 2);;

(* Points to Make:
  Note that with this function, the assumption is that 
  arguments are integers, so you don't have to deal with floats. 
  1. both x and p are of type int
  Type of the Function: int -> int -> int *)
let rec pow x p =
  if (p = 0) then
    1
  else
    x * pow x (p - 1);;

(* log function *)
let rec log x y =
  if (y = x) then
    1
  else
    if (y < x) then
      0
    else
      1 + log x (y / x);;

(************* THIS IS THE BEGINNING OF THE WORK FOR THE gcf function *****************)
(* Recursive Helper Function for gcf x y
// This function finds the factorization of n *)
let rec factorization n p lst =
  if (p = n) then
    p :: lst
  else  
    if ((n mod p) = 0) then
      (factorization n (p + 1) (p :: lst))
    else
      (factorization n (p + 1) lst);;

(* in this function, determine the factorization of
   both x and y and afterwards determine which of those prime factors
   are identical and output them as the result *)
(* Recursive Helper Function for findcommon1 x_lst y_lst count_x *)
let rec findcommon2 x y_lst count_y = 
  if (count_y >= length_of_list y_lst) then
    false
  else
    let y_head = match y_lst with 
      [] -> 0
    | h::t -> h in
    let y_tail = match y_lst with 
      [] -> []
    | h::t -> t in
    if (x = y_head) then
      true
    else
      findcommon2 x y_tail (count_y + 1);;

(* Recursive Helper Function for gcf x y
// This function finds the greatest common element between two lists x_lst and y_lst *)
let rec findcommon1 x_lst y_lst count_x = 
  if (count_x >= length_of_list (x_lst)) then
    1
  else 
    let x_head = match x_lst with 
      [] -> 0
    | h::t -> h in
    let x_tail = match x_lst with 
      [] -> []
    | h::t -> t in
    if (not (findcommon2 x_head y_lst 1)) then
      findcommon1 x_tail y_lst (count_x + 1)
    else
      x_head;;

(* final graded gcf function *)
let gcf x y = 
  if (x = 0 || y = 0) then
    if (abs(x) > 0) then
      x
    else if (abs(y) > 0) then
      y
    else
      0
  else
    let x_factors = factorization x 1 [] in
    let y_factors = factorization y 1 [] in
    findcommon1 x_factors y_factors 1;;

(* a helper function that obtains everything in the list except the last element 
   (for future work) *)
let rec list_without_last_element lst = match lst with
| [] -> []
| h :: t -> 
  if (length_of_list lst = 2) then
    [h]
  else 
    h :: list_without_last_element t;;

(*****************)
(* Part 3: Lists *)
(*****************)
(* appends a element 'm' to the end of the list lst *)
(* helper function for reverse *)
let rec append_list lst m = match lst with 
  | [] -> [m]
  | h :: t -> h :: (append_list t m);;

(* reverses the elements in a list *)
let rec reverse lst = match lst with
  | [] -> []
  | x :: xs -> append_list (reverse xs) x;;

(* end of reverse function work *)
(* helper function for zip; this function combines two ('a * 'b) tuples *)
let combine_tuples tup1 tup2 = 
  match tup1 with
| (tup1_h, tup1_t) -> 
  match tup2 with
| (tup2_h, tup2_t) -> 
  (tup1_h, tup1_t, tup2_h, tup2_t);;

(* merges lst1 and lst2 *)
let rec zip lst1 lst2 = 
  if (length_of_list lst1 = 0 || length_of_list lst2 = 0) then
    []
  else 
    match lst1 with
    | h1 :: t1 ->
    match lst2 with
    | h2 :: t2 -> 
    (combine_tuples h1 h2) :: zip t1 t2;; 

(* graded function: merges two lists and returns the sorted version of the merged list *)
(* helper function #2 for merge [for merging two lists] *)
let rec merge_lists lst1 lst2 = 
  if (length_of_list lst2 = 0) then
    lst1
  else
    match lst2 with 
    | [] -> []
    | h :: t -> merge_lists (append_list lst1 h) t;;

(* helper function #1 for merge [for sorting and merging two given lists] *)
let rec sort_merge lst1 lst2 merged = 
  if (length_of_list lst1 = 0 && length_of_list lst2 = 0) then
    merged
  else if (length_of_list lst1 = 0) then
    merge_lists merged lst2
  else if (length_of_list lst2 = 0) then
    merge_lists merged lst1
  else if (first_element lst1 > first_element lst2) then
    let updated_merged1 = append_list merged (first_element lst2) in
    sort_merge lst1 (list_without_first_element lst2) updated_merged1
  else if (first_element lst1 < first_element lst2) then
    let updated_merged2 = append_list merged (first_element lst1) in
    sort_merge (list_without_first_element lst1) lst2 updated_merged2
  else
    sort_merge (list_without_first_element lst1) (list_without_first_element lst2) merged;;

(* final graded merge function *)
let rec merge lst1 lst2 = 
  sort_merge lst1 lst2 [];;
  
(* END OF THE WORK FOR THE MERGE FUNCTION *)

(* recursive helper function for every_nth *)
let rec find_nth count n lst =
  match lst with  
  | [] -> []
  | h :: t ->
    if (count mod n = 0) then 
      h :: (find_nth (count + 1) n t)
    else 
      (find_nth (count + 1) n t);;

let every_nth n lst = 
  find_nth 1 n lst;;
      
let rec is_present lst v = match lst with
  | [] -> false
  | h :: t -> (h = v) || is_present t v;;

(* ADVICE FOR jumping tuples *)
(* for the first half of the list, take every tuple in list1 
   and if it is odd indexed, take its first element and add
   it to the first half of the list; then take every tuple
   in list2 and if it is even indexed, take its second element
   and add it to the first half of the list *)

(* for the second half of the list, take every tuple in list1
   and if it is even indexed, take its first element and add
   it to the second half of the list; then take every tuple
   in list 2 and if it is odd indexed, take its second element
   and add it to the second half of the list *)

(* extraction helper functions *)
let rec extract_x_tuple tup = match tup with 
  | (x, y) -> x;;

let rec extract_y_tuple tup = match tup with 
  | (x, y) -> y;;
(* ^^ extraction helper functions ^^ *)

(* helper function for extracting the nth element of a list*)
let rec extract_nth_helper lst count n = 
  if (count = n) then
    match lst with
    | [x] -> x
    | h :: t -> h
  else
    match lst with
    | h :: t -> extract_nth_helper t (count + 1) n;;

(* extracts the nth element of a list *)
let rec extract_nth lst n =
  extract_nth_helper lst 0 n;;

(* processes the first half of the result lst for jumping_tuples *)
let rec jumping_first_half lst1 lst2 count_first =
  if (count_first > (length_of_list lst1) - 1) then
    []
  else
    if (is_even ((count_first) mod (length_of_list lst1))) then
      (extract_x_tuple (extract_nth lst1 count_first)) :: (jumping_first_half lst1 lst2 (count_first + 1)) 
    else
      (extract_y_tuple (extract_nth lst2 count_first)) :: (jumping_first_half lst1 lst2 (count_first + 1));;

(* processes the second half of the result lst for jumping_tuples *)
let rec jumping_second_half lst1 lst2 count_second =
  if (count_second > (length_of_list lst1) - 1) then
    []
  else
    if (is_even ((count_second) mod (length_of_list lst1))) then
      (extract_y_tuple (extract_nth lst2 count_second)) :: (jumping_second_half lst1 lst2 (count_second + 1))
    else
      (extract_x_tuple (extract_nth lst1 count_second)) :: (jumping_second_half lst1 lst2 (count_second + 1));;

(* graded jumping_tuples function *)
let jumping_tuples lst1 lst2 =
  merge_lists (jumping_second_half lst1 lst2 0) (jumping_first_half lst1 lst2 0);;

let rec max_func_chain init funcs = failwith "not implemented";;

(*****************)
(* Part 4: HOF *)
(*****************)

(* ignore the functions below *)
(*
let rec map f xs = match xs with
| [] -> []
| x :: xt -> (f x)::(map f xt)

let rec fold f a xs = match xs with
| [] -> a
| x :: xt -> fold f (f a x) xt

let rec fold_right f xs a = match xs with
| [] -> a
| x :: xt -> f x (fold_right f xt a)
*)
(* ignore the functions above *)

let both_equal x y = if (x = y) then true else false;;
let or_operator x y = x || y;;
let obtain_truth f x lst = map (both_equal x) lst;;

(* graded is_there function *)
let is_there lst x = 
  let truth = (obtain_truth both_equal x lst) in
  if (fold or_operator false truth) then
    true
  else
    false;;

(* helper functions for count_occ *)
let num_equal x y = if (both_equal x y) then 1 else 0;;
let list_of_truth lst x = map (num_equal x) lst;;
let add_helper x y = x + y;;

(* graded count_occ function *)
let count_occ lst target = 
  let updated_lst = list_of_truth lst target in 
  fold add_helper 0 updated_lst;;

let frequency_lst lst = map (count_occ lst) lst;;

let list_without_x lst x =
  if (count_occ lst x > 1) then 
    match lst with 
    | a::x::b -> a :: b
  else
    lst;;

let correct_list lst result x =
  if ((count_occ lst x >= 1) && (is_there result x = false)) then
    x :: result
  else
    result;;

let uniq_helper lst result = 
  fold (correct_list lst) result lst;;

(* final uniq function *)
let uniq lst = 
  uniq_helper lst [];;

let add_position_field x = (x, 1);;
let initialize_position lst = map add_position_field lst;;
let increment_tuple now prev =
  match prev with
  | (a, b) -> 
  match now with
  | (x, y) -> (x, b + 1);;
let update_positions lst =
  let new_lst = initialize_position lst in
  match new_lst with
  | h :: t -> fold (increment_tuple) h new_lst;;  

let count_th_element result x tup = 
  match tup with
  | (a, b) ->
  if (b mod x = 0) then
    tup :: result 
  else
    result;;

let every_xth_helper lst result x = 
  let new_lst = update_positions lst in
  fold (count_th_element result x) result new_lst;;

let every_xth x lst = 
  fold (every_xth_helper lst) [] lst;;





