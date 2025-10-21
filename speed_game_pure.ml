(* Speed Card Game - Pure OCaml Logic *)
(* This file contains the core game logic in pure OCaml *)

open! Core

(* Card module *)
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
    if Option.is_none pile_card then false
    else
      let pile_card = Option.value_exn pile_card in
      let card_val = rank_value card.rank in
      let pile_val = rank_value pile_card.rank in
      
      (* Ace is wild - can play on King or 2 *)
      if card.rank = Ace then
        pile_val = 13 || pile_val = 2  (* King or 2 *)
      else if pile_card.rank = Ace then
        card_val = 13 || card_val = 2  (* King or 2 *)
      else
        (* Normal ±1 rule *)
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

(* Move module *)
module Move = struct
  type t =
    | Play_card of { card : Card.t; pile : int }  (* pile: 0 or 1 *)
    | Draw_cards
  [@@deriving sexp, compare]
  
  let play_card card pile = Play_card { card; pile }
  let draw_cards = Draw_cards
end

(* Game state module *)
module GameState = struct
  type t = {
    player1_hand : Card.t list;
    player2_hand : Card.t list;
    player1_stock : Card.t list;
    player2_stock : Card.t list;
    pile1 : Card.t option;
    pile2 : Card.t option;
    current_player : string;  (* "Player1" or "Player2" *)
    game_over : bool;
    winner : string option;
  }
  [@@deriving sexp, compare, equal]
  
  let create () =
    (* Create deck *)
    let suits = [Card.Hearts; Card.Diamonds; Card.Clubs; Card.Spades] in
    let ranks = [Card.Ace; Card.Two; Card.Three; Card.Four; Card.Five; Card.Six; Card.Seven; Card.Eight; Card.Nine; Card.Ten; Card.Jack; Card.Queen; Card.King] in
    
    let deck = List.concat_map suits ~f:(fun suit ->
      List.map ranks ~f:(fun rank -> { Card.suit; rank })
    ) in
    
    (* Shuffle deck *)
    let shuffled_deck = List.permute deck in
    
    (* Deal cards *)
    {
      player1_hand = List.take shuffled_deck 5;
      player2_hand = List.take (List.drop shuffled_deck 5) 5;
      player1_stock = List.take (List.drop shuffled_deck 10) 15;
      player2_stock = List.take (List.drop shuffled_deck 25) 15;
      pile1 = List.nth shuffled_deck 40;
      pile2 = List.nth shuffled_deck 41;
      current_player = "Player1";
      game_over = false;
      winner = None;
    }
  
  let make_move game_state move =
    if game_state.game_over then
      Error "Game is over"
    else
      match move with
      | Move.Play_card { card; pile } ->
        let pile_card = if pile = 0 then game_state.pile1 else game_state.pile2 in
        if Option.is_none pile_card then
          Error "Empty pile"
        else if not (Card.can_play_on card pile_card) then
          Error "Invalid play"
        else
          let hand = if game_state.current_player = "Player1" then game_state.player1_hand else game_state.player2_hand in
          if not (List.mem hand card ~equal:Card.equal) then
            Error "Card not in hand"
          else
            let new_hand = List.filter hand ~f:(fun c -> not (Card.equal c card)) in
            let new_pile1 = if pile = 0 then Some card else game_state.pile1 in
            let new_pile2 = if pile = 1 then Some card else game_state.pile2 in
            
            (* Auto-draw to maintain 5 cards *)
            let stock = if game_state.current_player = "Player1" then game_state.player1_stock else game_state.player2_stock in
            let new_stock = List.drop stock 1 in
            let new_hand_with_draw = if List.length new_hand < 5 && List.length stock > 0 then
              List.take stock 1 @ new_hand
            else new_hand in
            
            (* Check for win *)
            let game_over = List.is_empty new_hand_with_draw && List.is_empty new_stock in
            let winner = if game_over then Some game_state.current_player else None in
            
            let new_player1_hand = if game_state.current_player = "Player1" then new_hand_with_draw else game_state.player1_hand in
            let new_player2_hand = if game_state.current_player = "Player2" then new_hand_with_draw else game_state.player2_hand in
            let new_player1_stock = if game_state.current_player = "Player1" then new_stock else game_state.player1_stock in
            let new_player2_stock = if game_state.current_player = "Player2" then new_stock else game_state.player2_stock in
            
            Ok {
              player1_hand = new_player1_hand;
              player2_hand = new_player2_hand;
              player1_stock = new_player1_stock;
              player2_stock = new_player2_stock;
              pile1 = new_pile1;
              pile2 = new_pile2;
              current_player = if game_state.current_player = "Player1" then "Player2" else "Player1";
              game_over;
              winner;
            }
      | Move.Draw_cards ->
        let stock = if game_state.current_player = "Player1" then game_state.player1_stock else game_state.player2_stock in
        let hand = if game_state.current_player = "Player1" then game_state.player1_hand else game_state.player2_hand in
        if List.is_empty stock then
          Error "No cards to draw"
        else if List.length hand >= 5 then
          Error "Hand is full"
        else
          let new_stock = List.drop stock 1 in
          let new_hand = List.hd_exn stock :: hand in
          let new_player1_hand = if game_state.current_player = "Player1" then new_hand else game_state.player1_hand in
          let new_player2_hand = if game_state.current_player = "Player2" then new_hand else game_state.player2_hand in
          let new_player1_stock = if game_state.current_player = "Player1" then new_stock else game_state.player1_stock in
          let new_player2_stock = if game_state.current_player = "Player2" then new_stock else game_state.player2_stock in
          
          Ok {
            player1_hand = new_player1_hand;
            player2_hand = new_player2_hand;
            player1_stock = new_player1_stock;
            player2_stock = new_player2_stock;
            pile1 = game_state.pile1;
            pile2 = game_state.pile2;
            current_player = if game_state.current_player = "Player1" then "Player2" else "Player1";
            game_over = game_state.game_over;
            winner = game_state.winner;
          }
  
  let get_all_moves game_state =
    let hand = if game_state.current_player = "Player1" then game_state.player1_hand else game_state.player2_hand in
    let moves = List.concat_map hand ~f:(fun card ->
      let pile1_moves = if Option.is_some game_state.pile1 && Card.can_play_on card game_state.pile1 then
        [Move.play_card card 0]
      else [] in
      let pile2_moves = if Option.is_some game_state.pile2 && Card.can_play_on card game_state.pile2 then
        [Move.play_card card 1]
      else [] in
      pile1_moves @ pile2_moves
    ) in
    let stock = if game_state.current_player = "Player1" then game_state.player1_stock else game_state.player2_stock in
    let draw_moves = if not (List.is_empty stock) && List.length hand < 5 then
      [Move.draw_cards]
    else [] in
    moves @ draw_moves
  
  let are_both_players_stuck game_state =
    let player1_can_play = List.exists game_state.player1_hand ~f:(fun card ->
      (Option.is_some game_state.pile1 && Card.can_play_on card game_state.pile1) ||
      (Option.is_some game_state.pile2 && Card.can_play_on card game_state.pile2)
    ) in
    let player2_can_play = List.exists game_state.player2_hand ~f:(fun card ->
      (Option.is_some game_state.pile1 && Card.can_play_on card game_state.pile1) ||
      (Option.is_some game_state.pile2 && Card.can_play_on card game_state.pile2)
    ) in
    not player1_can_play && not player2_can_play
  
  let change_middle_cards game_state =
    (* Create new random cards *)
    let suits = [Card.Hearts; Card.Diamonds; Card.Clubs; Card.Spades] in
    let ranks = [Card.Ace; Card.Two; Card.Three; Card.Four; Card.Five; Card.Six; Card.Seven; Card.Eight; Card.Nine; Card.Ten; Card.Jack; Card.Queen; Card.King] in
    
    let random_suit () = List.random_element_exn suits in
    let random_rank () = List.random_element_exn ranks in
    
    let new_card1 = { Card.suit = random_suit (); rank = random_rank () } in
    let new_card2 = { Card.suit = random_suit (); rank = random_rank () } in
    
    { game_state with pile1 = Some new_card1; pile2 = Some new_card2 }
end

(* Export functions for JavaScript *)
let create_game () = GameState.create ()
let make_move game_state move = GameState.make_move game_state move
let get_all_moves game_state = GameState.get_all_moves game_state
let are_both_players_stuck game_state = GameState.are_both_players_stuck game_state
let change_middle_cards game_state = GameState.change_middle_cards game_state
let card_can_play_on card pile_card = Card.can_play_on card pile_card
let card_to_string card = Card.to_string card
