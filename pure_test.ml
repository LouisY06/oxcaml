(* Simple Speed game test without Core dependencies *)
module Card = struct
  type suit = Hearts | Diamonds | Clubs | Spades
  type rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
  type t = { suit : suit; rank : rank }
  
  let rank_value = function
    | Ace -> 1 | Two -> 2 | Three -> 3 | Four -> 4 | Five -> 5 | Six -> 6 | Seven -> 7
    | Eight -> 8 | Nine -> 9 | Ten -> 10 | Jack -> 11 | Queen -> 12 | King -> 13
  
  let can_play_on card pile_card =
    let card_val = rank_value card.rank in
    let pile_val = rank_value pile_card.rank in
    card_val = pile_val + 1 || card_val = pile_val - 1
  
  let to_string { suit; rank } =
    let suit_str = match suit with
      | Hearts -> "♥" | Diamonds -> "♦" | Clubs -> "♣" | Spades -> "♠"
    in
    let rank_str = match rank with
      | Ace -> "A" | Two -> "2" | Three -> "3" | Four -> "4" | Five -> "5"
      | Six -> "6" | Seven -> "7" | Eight -> "8" | Nine -> "9" | Ten -> "10"
      | Jack -> "J" | Queen -> "Q" | King -> "K"
    in
    rank_str ^ suit_str
end

let () =
  print_endline "=== Speed Card Game - OCaml Implementation ===";
  print_endline "";
  
  (* Test card creation and display *)
  let ace_hearts = { Card.suit = Card.Hearts; rank = Card.Ace } in
  let king_spades = { Card.suit = Card.Spades; rank = Card.King } in
  let two_clubs = { Card.suit = Card.Clubs; rank = Card.Two } in
  
  print_endline "Card Examples:";
  print_endline ("Ace of Hearts: " ^ Card.to_string ace_hearts);
  print_endline ("King of Spades: " ^ Card.to_string king_spades);
  print_endline ("Two of Clubs: " ^ Card.to_string two_clubs);
  print_endline "";
  
  (* Test card playability *)
  print_endline "Card Playability Tests:";
  print_endline ("Can Ace play on King? " ^ (if Card.can_play_on ace_hearts king_spades then "Yes" else "No"));
  print_endline ("Can Ace play on Two? " ^ (if Card.can_play_on ace_hearts two_clubs then "Yes" else "No"));
  print_endline ("Can King play on Ace? " ^ (if Card.can_play_on king_spades ace_hearts then "Yes" else "No"));
  print_endline "";
  
  print_endline "=== OCaml Speed Game Logic Working! ==="
