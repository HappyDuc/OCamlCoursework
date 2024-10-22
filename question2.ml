(** CW2, QUESTION 2 **)

(* a (3 marks) *)

(* Provide definitions below! Remember to include comments to justify your solutions. *)

(* Type of the suits that a card can be *)
type suit = Hearts | Clubs | Diamonds | Spades | OtherSuit

(* Helper type of values that a card can be *)
type cardValue = Number of int | Ten | Jack | Queen | King | Ace | OtherCardValue

(* A pair of the suit and the cardValue. *)
type card = Card of (suit * cardValue)

(* validSuit : suit -> bool *)
let validSuit suit =
  match suit with
  | Hearts -> true
  | Clubs -> true
  | Diamonds -> true
  | Spades -> true
  | OtherSuit -> false

(* validCardValue : cardValue -> bool *)
let validCardValue cardValue =
  match cardValue with
  | Number n -> 1 < n && n < 10
  | Ten -> true
  | Jack -> true
  | Queen -> true
  | King -> true
  | Ace -> true
  | OtherCardValue -> false

(* validCard : card -> bool *)

let validCard card =
  match card with
  | (_, OtherCardValue) -> false
  | (OtherSuit, _) -> false
  | (suit, cardValue) -> validSuit suit && validCardValue cardValue

(* b (3 marks) *)

(* parseSuit : string -> suit *)
exception Invalid_argument
let parseSuit string =
  match string with
  | "H" -> Hearts
  | "C" -> Clubs
  | "D" -> Diamonds
  | "S" -> Spades
  | _ -> raise Invalid_argument

(* parseCardValue : string -> cardValue*)
(* Function to parse the cardValue from a string.*)
let parseCardValue string =
  match string with
  | "N" -> Number 1
  | "T" -> Ten
  | "J" -> Jack
  | "Q" -> Queen
  | "K" -> King
  | "A" -> Ace
  | _ -> OtherCardValue

(* parseCard : string -> card *)
let parseCard cardInput = 
  let newSuit = parseSuit (String.sub cardInput 0 1) in
  let newCardValue = parseCardValue (String.sub cardInput 1 1) in
  if validCard (newSuit, newCardValue) then
    Card (newSuit, newCardValue)
  else
    raise Invalid_argument

(* c (4 marks) *)

(* winner : suit option -> card list -> int *)
let winner trumpSuit cards = 
  if trumpSuit = None then
    let trumpCard = match cards with
    | x :: xs -> parseCard(x)
    | [] -> failwith "Card List is empty"
    in let trumpSuit = match trumpCard with
      | Card (suit, _) -> suit
      in ()
  else
    ()


