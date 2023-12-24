open Aoc.Util

type hand = { cards : int list; bid : int }

let parse_card = function
  | 'T' -> 10
  | 'J' -> 11
  | 'Q' -> 12
  | 'K' -> 13
  | 'A' -> 14
  | ch -> int_of_string (String.make 1 ch)

let parse_card2 = function
  | 'T' -> 10
  | 'J' -> 0
  | 'Q' -> 12
  | 'K' -> 13
  | 'A' -> 14
  | ch -> int_of_string (String.make 1 ch)

(* `parse_cards "32T3K"` returns `[3; 2; 10; 3; 13]` *)
let parse_cards cards_str =
  String.fold_right (fun ch cards -> parse_card ch :: cards) cards_str []

(* `parse_cards "32T3K"` returns `[3; 2; 10; 3; 13]` *)
let parse_cards2 cards_str =
  String.fold_right (fun ch cards -> parse_card2 ch :: cards) cards_str []

let parse_line line =
  match String.split_on_char ' ' line with
  | [ cards_str; bid_str ] ->
      { cards = parse_cards cards_str; bid = int_of_string bid_str }
  | _ ->
      print_endline "Can't parse line:";
      print_endline line;
      exit 1

let parse_line2 line =
  match String.split_on_char ' ' line with
  | [ cards_str; bid_str ] ->
      { cards = parse_cards2 cards_str; bid = int_of_string bid_str }
  | _ ->
      print_endline "Can't parse line:";
      print_endline line;
      exit 1

let count_cards arr = List.iter (fun n -> arr.(n) <- arr.(n) + 1)
let array_max = Array.fold_left Int.max Int.min_int

let array_nonzero =
  Array.fold_left (fun count n -> if n != 0 then count + 1 else count) 0

let hand_type hand =
  let arr = Array.make 15 0 in
  count_cards arr hand.cards;
  match array_max arr with
  | 5 -> 6 (* Five of a kind *)
  | 4 -> 5 (* Four of a kind *)
  | 3 ->
      if array_nonzero arr == 2 then 4 (* 2 distinct cards => full house *)
      else 3 (* 3 distinct cards => three of a kind *)
  | 2 ->
      if array_nonzero arr == 3 then 2 (* 3 distinct cards => two pair *)
      else 1 (* 4 distinct cards => one pair *)
  | _ -> 0 (* High card *)

let hand_type2 hand =
  let arr = Array.make 15 0 in
  count_cards arr hand.cards;
  let non_jokers = Array.sub arr 1 14 and jokers = arr.(0) in
  match array_max non_jokers + jokers with
  | 5 -> 6 (* Five of a kind *)
  | 4 -> 5 (* Four of a kind *)
  | 3 ->
      if array_nonzero non_jokers <= 2 then 4 (* 2 distinct cards => full house *)
      else 3 (* 3 distinct cards => three of a kind *)
  | 2 ->
      if array_nonzero non_jokers <= 3 then 2 (* 3 distinct cards => two pair *)
      else 1 (* 4 distinct cards => one pair *)
  | _ -> 0 (* High card *)

let rec cmp_cards l1 l2 =
  match (l1, l2) with
  | m :: l1, n :: l2 -> if m == n then cmp_cards l1 l2 else m - n
  | _ -> 0

let cmp_hands hand1 hand2 =
  match hand_type hand1 - hand_type hand2 with
  | 0 -> cmp_cards hand1.cards hand2.cards
  | x -> x

let cmp_hands2 hand1 hand2 =
  match hand_type2 hand1 - hand_type2 hand2 with
  | 0 -> cmp_cards hand1.cards hand2.cards
  | x -> x

let rec parse_input () =
  match readl () with Some line -> parse_line line :: parse_input () | _ -> []

let rec parse_input2 () =
  match readl () with Some line -> parse_line2 line :: parse_input2 () | _ -> []

let rank_hands hands =
  List.mapi (fun i hand -> (i + 1, hand)) (List.sort cmp_hands hands)

let rank_hands2 hands =
  List.mapi (fun i hand -> (i + 1, hand)) (List.sort cmp_hands2 hands)

let part1 () =
  List.fold_left
    (fun acc (rank, hand) -> acc + (rank * hand.bid))
    0
    (rank_hands (parse_input ()))

let part2 () =
  List.fold_left
    (fun acc (rank, hand) -> acc + (rank * hand.bid))
    0
    (rank_hands2 (parse_input2 ()))
