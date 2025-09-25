open! Core

(* Card representation *)
module Card = struct
  type suit = Hearts | Diamonds | Clubs | Spades
  [@@deriving sexp, compare, equal]

  type rank = 
    | Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
    | Jack | Queen | King
  [@@deriving sexp, compare, equal]

  type t = { suit : suit; rank : rank }
  [@@deriving sexp, compare, equal]

  let rank_value = function
    | Ace -> 1
    | Two -> 2 | Three -> 3 | Four -> 4 | Five -> 5 | Six -> 6 | Seven -> 7
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

(* Player representation *)
module Player = struct
  type t = 
    | Player1 
    | Player2
  [@@deriving sexp, compare, equal]

  let opposite = function
    | Player1 -> Player2
    | Player2 -> Player1
end

(* Move types *)
module Move = struct
  type t =
    | Play_card of { card : Card.t; pile : int }  (* pile: 0 or 1 *)
    | Draw_cards  (* Draw from stock pile *)
  [@@deriving sexp, compare]
end

(* Game state *)
module Game_state = struct
  type t = {
    (* Player hands - each player has 5 cards in hand *)
    player1_hand : Card.t list;
    player2_hand : Card.t list;
    
    (* Central piles - 2 piles where cards are played *)
    pile1 : Card.t option;  (* Top card of pile 1 *)
    pile2 : Card.t option;  (* Top card of pile 2 *)
    
    (* Stock piles - each player has a stock pile to draw from *)
    player1_stock : Card.t list;
    player2_stock : Card.t list;
    
    (* Current player *)
    current_player : Player.t;
    
    (* Game status *)
    game_over : bool;
    winner : Player.t option;
  }
  [@@deriving sexp, compare, equal]

  module Move_error = struct
    type t =
      | Game_is_over
      | Not_your_turn
      | Card_not_in_hand
      | Invalid_play  (* Card cannot be played on the specified pile *)
      | Empty_pile    (* Trying to play on empty pile *)
      | No_cards_to_draw
    [@@deriving sexp, compare]
  end

  (* Create initial game state *)
  let create () : t =
    (* Create a standard 52-card deck *)
    let suits = [Card.Hearts; Card.Diamonds; Card.Clubs; Card.Spades] in
    let ranks = [Card.Ace; Card.Two; Card.Three; Card.Four; Card.Five; Card.Six; Card.Seven;
                 Card.Eight; Card.Nine; Card.Ten; Card.Jack; Card.Queen; Card.King] in
    let deck = 
      List.cartesian_product suits ranks
      |> List.map ~f:(fun (suit, rank) -> { Card.suit; rank })
    in
    
    (* Shuffle deck (simplified - just reverse for demo) *)
    let shuffled_deck = List.rev deck in
    
    (* Deal cards: 5 to each hand, rest split between stock piles *)
    let player1_hand = List.take shuffled_deck 5 in
    let remaining = List.drop shuffled_deck 5 in
    let player2_hand = List.take remaining 5 in
    let final_remaining = List.drop remaining 5 in
    
    (* Split remaining cards between stock piles *)
    let mid = List.length final_remaining / 2 in
    let player1_stock = List.take final_remaining mid in
    let player2_stock = List.drop final_remaining mid in
    
    {
      player1_hand;
      player2_hand;
      pile1 = None;
      pile2 = None;
      player1_stock;
      player2_stock;
      current_player = Player.Player1;
      game_over = false;
      winner = None;
    }

  (* Check if a card can be played on a pile *)
  let can_play_card card pile_card =
    match pile_card with
    | None -> true  (* Can play any card on empty pile *)
    | Some pile_top -> Card.can_play_on card pile_top

  (* Remove card from hand *)
  let remove_card_from_hand hand card =
    List.filter hand ~f:(fun c -> not (Card.equal c card))

  (* Check if player has won (no cards in hand or stock) *)
  let has_won player game_state =
    match player with
    | Player.Player1 -> 
        List.is_empty game_state.player1_hand && List.is_empty game_state.player1_stock
    | Player.Player2 -> 
        List.is_empty game_state.player2_hand && List.is_empty game_state.player2_stock

  (* Draw cards from stock to hand (up to 5 cards in hand) *)
  let draw_cards player game_state =
    let current_hand, current_stock = 
      match player with
      | Player.Player1 -> (game_state.player1_hand, game_state.player1_stock)
      | Player.Player2 -> (game_state.player2_hand, game_state.player2_stock)
    in
    
    let cards_needed = 5 - List.length current_hand in
    let cards_to_draw = min cards_needed (List.length current_stock) in
    
    if cards_to_draw = 0 then
      Error Move_error.No_cards_to_draw
    else
      let drawn_cards = List.take current_stock cards_to_draw in
      let new_hand = current_hand @ drawn_cards in
      let new_stock = List.drop current_stock cards_to_draw in
      
      let new_game_state = 
        match player with
        | Player.Player1 -> 
            { game_state with 
              player1_hand = new_hand; 
              player1_stock = new_stock;
              current_player = Player.opposite player;
            }
        | Player.Player2 -> 
            { game_state with 
              player2_hand = new_hand; 
              player2_stock = new_stock;
              current_player = Player.opposite player;
            }
      in
      Ok new_game_state

  (* Main make_move function *)
  let make_move game_state (move : Move.t) : (t, Move_error.t) Result.t =
    if game_state.game_over then
      Error Move_error.Game_is_over
    else
      match move with
      | Move.Draw_cards ->
          draw_cards game_state.current_player game_state
          
      | Move.Play_card { card; pile } ->
          (* Validate it's the player's turn *)
          let current_hand = 
            match game_state.current_player with
            | Player.Player1 -> game_state.player1_hand
            | Player.Player2 -> game_state.player2_hand
          in
          
          (* Check if card is in hand *)
          if not (List.mem current_hand card ~equal:Card.equal) then
            Error Move_error.Card_not_in_hand
          else
            (* Check if pile is valid *)
            let pile_card = 
              match pile with
              | 0 -> game_state.pile1
              | 1 -> game_state.pile2
              | _ -> None
            in
            
            if pile < 0 || pile > 1 then
              Error Move_error.Empty_pile
            else
              (* Check if card can be played *)
              if not (can_play_card card pile_card) then
                Error Move_error.Invalid_play
              else
                (* Make the move *)
                let new_hand = remove_card_from_hand current_hand card in
                let new_pile_card = Some card in
                
                let new_game_state = 
                  match game_state.current_player, pile with
                  | Player.Player1, 0 -> 
                      { game_state with 
                        player1_hand = new_hand; 
                        pile1 = new_pile_card;
                        current_player = Player.Player2;
                      }
                  | Player.Player1, 1 -> 
                      { game_state with 
                        player1_hand = new_hand; 
                        pile2 = new_pile_card;
                        current_player = Player.Player2;
                      }
                  | Player.Player2, 0 -> 
                      { game_state with 
                        player2_hand = new_hand; 
                        pile1 = new_pile_card;
                        current_player = Player.Player1;
                      }
                  | Player.Player2, 1 -> 
                      { game_state with 
                        player2_hand = new_hand; 
                        pile2 = new_pile_card;
                        current_player = Player.Player1;
                      }
                  | _ -> game_state  (* Should not happen *)
                in
                
                (* Check for winner *)
                let winner = 
                  if has_won Player.Player1 new_game_state then Some Player.Player1
                  else if has_won Player.Player2 new_game_state then Some Player.Player2
                  else None
                in
                
                let final_game_state = 
                  { new_game_state with 
                    game_over = Option.is_some winner;
                    winner;
                  }
                in
                
                Ok final_game_state

  (* Get all possible moves for current player *)
  let get_all_moves game_state : Move.t list =
    if game_state.game_over then
      []
    else
      let current_hand = 
        match game_state.current_player with
        | Player.Player1 -> game_state.player1_hand
        | Player.Player2 -> game_state.player2_hand
      in
      
      let play_moves = 
        List.concat_map current_hand ~f:(fun card ->
          List.filter_map [0; 1] ~f:(fun pile ->
            let pile_card = 
              match pile with
              | 0 -> game_state.pile1
              | 1 -> game_state.pile2
              | _ -> None
            in
            if can_play_card card pile_card then
              Some (Move.Play_card { card; pile })
            else
              None
          )
        )
      in
      
      let draw_move = 
        let current_stock = 
          match game_state.current_player with
          | Player.Player1 -> game_state.player1_stock
          | Player.Player2 -> game_state.player2_stock
        in
        if not (List.is_empty current_stock) then
          [Move.Draw_cards]
        else
          []
      in
      
      play_moves @ draw_move

  (* Helper function to display game state *)
  let to_string game_state =
    let hand_to_string hand = 
      List.map hand ~f:Card.to_string |> String.concat ~sep:" "
    in
    let pile_to_string pile = 
      match pile with
      | None -> "Empty"
      | Some card -> Card.to_string card
    in
    let current_player_str = 
      match game_state.current_player with
      | Player.Player1 -> "Player 1"
      | Player.Player2 -> "Player 2"
    in
    let winner_str = 
      match game_state.winner with
      | None -> "Game in progress"
      | Some Player.Player1 -> "Player 1 wins!"
      | Some Player.Player2 -> "Player 2 wins!"
    in
    
    Printf.sprintf 
      "Current Player: %s\n\
       Player 1 Hand: %s\n\
       Player 2 Hand: %s\n\
       Pile 1: %s\n\
       Pile 2: %s\n\
       Player 1 Stock: %d cards\n\
       Player 2 Stock: %d cards\n\
       Status: %s"
      current_player_str
      (hand_to_string game_state.player1_hand)
      (hand_to_string game_state.player2_hand)
      (pile_to_string game_state.pile1)
      (pile_to_string game_state.pile2)
      (List.length game_state.player1_stock)
      (List.length game_state.player2_stock)
      winner_str
end

(* Example usage and test functions *)
let initial_state = Game_state.create ()

let example_move = Move.Play_card { 
  card = { Card.suit = Card.Hearts; rank = Card.Ace }; 
  pile = 0 
}

let example_draw_move = Move.Draw_cards

(* Test function to demonstrate the game *)
let test_game () =
  let game_state = Game_state.create () in
  Printf.printf "Initial game state:\n%s\n\n" (Game_state.to_string game_state);
  
  let moves = Game_state.get_all_moves game_state in
  Printf.printf "Available moves: %d\n" (List.length moves);
  
  (* Try to make a move if possible *)
  match moves with
  | move :: _ -> 
      (match Game_state.make_move game_state move with
       | Ok new_state -> 
           Printf.printf "Move successful!\n%s\n" (Game_state.to_string new_state)
       | Error err -> 
           Printf.printf "Move failed: %s\n" 
             (Sexp.to_string (Game_state.Move_error.sexp_of_t err)))
  | [] -> 
      Printf.printf "No moves available\n"
