(** CW2, QUESTION 2 **)

(* a (3 marks) *)

(* Provide definitions below! Remember to include comments to justify your solutions. *)
type suit = Hearts | Clubs | Diamonds | Spades | Other

type cardValue = Number of int | Ten | Jack | Queen | King | Ace | Other

(* Any number card is of the primitive type int and the face cards can be either
a jack, queen or king. I have chosen to not implement the joker card commonly 
found in real packs as they are not typically used. *)
type card = (suit * cardValue)

(* validSuit : suit -> bool *)
let validSuit suit =
  match suit with
  | Hearts -> true
  | Clubs -> true
  | Diamonds -> true
  | Spades -> true
  | Other -> false

let validCardValue cardValue =
  match cardValue with
  | Number n -> 1 < n && n < 10
  | Ten -> true
  | Jack -> true
  | Queen -> true
  | King -> true
  | Ace -> true
  | Other -> false

(* validCard : card -> bool *)

let validCard card =
  match card with
  | (suit, cardValue) -> validSuit suit && validCardValue cardValue

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

