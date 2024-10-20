(** Question 1 **)

(* Helper Functions *)

(*
Traverses the list, incrementing by 1 each time until the list is empty
O(N)
*)
let length xs =
  let rec helper count xs =
    match xs with
    | [] -> count
    | x :: xs -> helper (count + 1) xs
  in helper 0 xs

(* a *)
(* shiftL : int -> 'a list -> 'a list *)
(*
Finds the remainder of shifts/length, to save unnecessary operations.
Until the counter reaches the desired number of shifts:
  Add the head to the tail of the list and repeat.
O(N)
*)
let shiftL n xs =
  let n = n mod (length xs) in
  let rec helper count n xs =
    if count = n then
      xs
    else 
      match xs with
      | [] -> xs
      | x :: xs -> helper (count + 1) n (xs @ [x])
  in helper 0 n xs

(* shiftR : int -> 'a list -> 'a list *)
(*
Finds the remainder of shifts/length, to save unnecessary operations.
Until the counter reaches the desired number of shifts:
  
O(N)
*)
let shiftR n xs =
  let n = ((length xs) - n) in 
  let rec helper count n left right =
    if count = n then
      left @ right
    else 
      match left with
      | [] -> right @ left
      | head :: remaining -> helper (count + 1) n remaining (right @ [head])
  in (helper 0 n xs [])

(* b *)
(* isShiftedL : 'a list -> 'a list -> bool * int *)
let isShiftedL xs ys =
  let listLength = length xs in 
  if (listLength) = (length ys) then
    let rec helper count xs ys =
      if count = listLength then
        (false, -1)
      else
        if (shiftR count ys) = xs then
          (true, count)
        else helper (count + 1) xs ys
    in helper 0 xs ys
  else (false, -1)
  

