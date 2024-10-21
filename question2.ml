(** CW2, QUESTION 2 **)

(* a (3 marks) *)

(* Provide definitions below! Remember to include comments to justify your solutions. *)
(*  *)
type suit = HEARTS | CLUBS | DIAMONDS | SPADES

(* Any number card is of the primitive type int and the face cards can be either
a jack, queen or king. I have chosen to not implement the joker card commonly 
found in real packs as they are not typically used. *)
type card = ACE | NUMBER of int | TEN | JACK | QUEEN | KING

(* validSuit : suit -> bool *)
let validSuit value =
  match value with
  | HEARTS -> true
  | CLUBS -> true
  | DIAMONDS -> true
  | SPADES -> true


(* validCard : card -> bool *)

(* FIX THIS TO INCLUDE ALL CARDS #########################################*)

let validCard value =
  match value with
  | ACE -> true
  | NUMBER number -> if 1 < number && number < 10 then true else false
  | JACK -> true
  | QUEEN -> true
  | KING -> true
  | _ -> false

(* b (3 marks) *)

(* parseSuit : string -> suit *)
exception Invalid_argument
let parseSuit string =
  if String.length string = 1 then
    match string with
    | "HEARTS" -> HEARTS
    | "CLUBS" -> CLUBS
    | "DIAMONDS" -> DIAMONDS
    | "SPADES" -> SPADES
    | _ -> raise Invalid_argument
  else
    

(* parseCard : string -> card *)
let parseCard = failwith "Not implemented"

(* c (4 marks) *)

(* winner : suit option -> card list -> int *)
let winner = failwith "Not implemented"

