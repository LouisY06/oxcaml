open! Core
open Hw1

(* HW2: Enhanced Speed Card Game Logic *)
(* This module extends the core game logic with advanced features for simultaneous play *)

module Card = Hw1.Card
module Player = Hw1.Player
module Move = Hw1.Move
module Game_state = Hw1.Game_state

(* Enhanced game state for simultaneous play *)
module Enhanced_game_state = struct
  type t =
    { (* Core game state *)
      base_state : Game_state.t
    ; (* Enhanced features *)
      game_log : string list
    ; simultaneous_mode : bool
    ; ai_thinking : bool
    ; stuck_check_interval : bool
    }
  [@@deriving sexp, compare, equal]

  let create () =
    { base_state = Game_state.create ()
    ; game_log = [ "Game started! Click 'New Game' to begin." ]
    ; simultaneous_mode = true
    ; ai_thinking = false
    ; stuck_check_interval = false
    }
  ;;

  (* Make move with enhanced logging - SIMULTANEOUS MODE (no turns!) *)
  let make_move enhanced_state (move : Move.t) (player : string) : (t, string) Result.t =
    (* In simultaneous mode, we need to handle moves differently *)
    let base = enhanced_state.base_state in
    
    (* Determine which player is making the move *)
    let player_enum = 
      match player with
      | "Player1" -> Player.Player1
      | "Player2" -> Player.Player2
      | _ -> Player.Player1
    in
    
    (* Debug: print current hand *)
    let current_hand = 
      match player_enum with
      | Player.Player1 -> base.player1_hand
      | Player.Player2 -> base.player2_hand
    in
    let () = 
      match move with
      | Move.Play_card { card; pile = _ } ->
        Stdio.printf "🔍 Checking if card %s is in hand. Hand has %d cards:\n" 
          (Card.to_string card) (List.length current_hand);
        List.iteri current_hand ~f:(fun i c ->
          Stdio.printf "  [%d] %s (equal? %b)\n" i (Card.to_string c) (Card.equal c card));
        Stdio.printf "%!"
      | _ -> ()
    in
    
    (* For simultaneous mode, temporarily set current_player to the acting player *)
    let temp_state = { base with current_player = player_enum } in
    let result = Game_state.make_move temp_state move in
    
    match result with
    | Ok new_base_state ->
      let move_str =
        match move with
        | Move.Play_card { card; pile } ->
          Printf.sprintf "%s played %s on pile %d" player (Card.to_string card) (pile + 1)
        | Move.Draw_cards -> Printf.sprintf "%s drew cards" player
      in
      let new_log = move_str :: enhanced_state.game_log in
      (* Check for win condition *)
      let win_msg =
        if new_base_state.game_over
        then (
          match new_base_state.winner with
          | Some Player.Player1 -> "🎉 Player 1 wins! 🎉"
          | Some Player.Player2 -> "🎉 Player 2 wins! 🎉"
          | None -> "")
        else ""
      in
      let final_log = if String.is_empty win_msg then new_log else win_msg :: new_log in
      Ok { enhanced_state with base_state = new_base_state; game_log = final_log }
    | Error err ->
      let error_msg =
        match err with
        | Game_state.Move_error.Game_is_over -> "Game is over"
        | Game_state.Move_error.Not_your_turn -> "Not your turn"
        | Game_state.Move_error.Card_not_in_hand -> "Card not in hand"
        | Game_state.Move_error.Invalid_play -> "Invalid play"
        | Game_state.Move_error.Empty_pile -> "Empty pile"
        | Game_state.Move_error.No_cards_to_draw -> "No cards to draw"
      in
      Error error_msg
  ;;

  (* Get all moves for a specific player *)
  let get_all_moves enhanced_state (player : string) : Move.t list =
    if enhanced_state.base_state.game_over
    then []
    else (
      let current_hand =
        match player with
        | "Player1" -> enhanced_state.base_state.player1_hand
        | "Player2" -> enhanced_state.base_state.player2_hand
        | _ -> []
      in
      let play_moves =
        List.concat_map current_hand ~f:(fun card ->
          List.filter_map [ 0; 1 ] ~f:(fun pile ->
            let pile_card =
              match pile with
              | 0 -> enhanced_state.base_state.pile1
              | 1 -> enhanced_state.base_state.pile2
              | _ -> None
            in
            if Card.can_play_on card pile_card
            then Some (Move.Play_card { card; pile })
            else None))
      in
      let draw_move =
        let current_stock =
          match player with
          | "Player1" -> enhanced_state.base_state.player1_stock
          | "Player2" -> enhanced_state.base_state.player2_stock
          | _ -> []
        in
        let current_hand_size = List.length current_hand in
        if (not (List.is_empty current_stock)) && current_hand_size < 5
        then [ Move.Draw_cards ]
        else []
      in
      play_moves @ draw_move)
  ;;

  (* Check if both players are stuck *)
  let are_both_players_stuck enhanced_state =
    let player1_can_play =
      List.exists enhanced_state.base_state.player1_hand ~f:(fun card ->
        Card.can_play_on card enhanced_state.base_state.pile1
        || Card.can_play_on card enhanced_state.base_state.pile2)
    in
    let player2_can_play =
      List.exists enhanced_state.base_state.player2_hand ~f:(fun card ->
        Card.can_play_on card enhanced_state.base_state.pile1
        || Card.can_play_on card enhanced_state.base_state.pile2)
    in
    (not player1_can_play) && not player2_can_play
  ;;

  (* Refresh center cards when both players are stuck *)
  let refresh_center_cards enhanced_state =
    let suits = [ Card.Hearts; Card.Diamonds; Card.Clubs; Card.Spades ] in
    let ranks =
      [ Card.Ace
      ; Card.Two
      ; Card.Three
      ; Card.Four
      ; Card.Five
      ; Card.Six
      ; Card.Seven
      ; Card.Eight
      ; Card.Nine
      ; Card.Ten
      ; Card.Jack
      ; Card.Queen
      ; Card.King
      ]
    in
    let random_suit () = List.random_element_exn suits in
    let random_rank () = List.random_element_exn ranks in
    let new_card1 = { Card.suit = random_suit (); rank = random_rank () } in
    let new_card2 = { Card.suit = random_suit (); rank = random_rank () } in
    let new_base_state =
      { enhanced_state.base_state with pile1 = Some new_card1; pile2 = Some new_card2 }
    in
    let refresh_msg =
      Printf.sprintf
        "🔄 Center cards refreshed: %s, %s"
        (Card.to_string new_card1)
        (Card.to_string new_card2)
    in
    { enhanced_state with
      base_state = new_base_state
    ; game_log = refresh_msg :: enhanced_state.game_log
    }
  ;;

  (* Check for immediate win condition *)
  let check_for_immediate_win enhanced_state =
    let player1_won =
      List.is_empty enhanced_state.base_state.player1_hand
      && List.is_empty enhanced_state.base_state.player1_stock
    in
    let player2_won =
      List.is_empty enhanced_state.base_state.player2_hand
      && List.is_empty enhanced_state.base_state.player2_stock
    in
    if player1_won
    then (
      let win_msg = "🎉 Player 1 wins! Player 1 ran out of cards! 🎉" in
      Some
        { enhanced_state with
          base_state =
            { enhanced_state.base_state with
              game_over = true
            ; winner = Some Player.Player1
            }
        ; game_log = win_msg :: enhanced_state.game_log
        })
    else if player2_won
    then (
      let win_msg = "🎉 Player 2 wins! Player 2 ran out of cards! 🎉" in
      Some
        { enhanced_state with
          base_state =
            { enhanced_state.base_state with
              game_over = true
            ; winner = Some Player.Player2
            }
        ; game_log = win_msg :: enhanced_state.game_log
        })
    else None
  ;;

  (* AI strategy: choose best move *)
  let ai_choose_move enhanced_state =
    let available_moves = get_all_moves enhanced_state "Player2" in
    if List.is_empty available_moves
    then None
    else (
      (* Simple AI: prefer playing cards over drawing *)
      let play_moves =
        List.filter available_moves ~f:(function
          | Move.Play_card _ -> true
          | Move.Draw_cards -> false)
      in
      if not (List.is_empty play_moves)
      then Some (List.hd_exn play_moves)
      else Some (List.hd_exn available_moves))
  ;;

  (* Display enhanced game state *)
  let to_string enhanced_state =
    let base_str = Game_state.to_string enhanced_state.base_state in
    let log_str = String.concat ~sep:"\n" (List.take enhanced_state.game_log 5) in
    Printf.sprintf "%s\n\nRecent moves:\n%s" base_str log_str
  ;;
end

(* Export functions for JavaScript integration *)
let create_enhanced_game () = Enhanced_game_state.create ()

let make_enhanced_move enhanced_state move player =
  Enhanced_game_state.make_move enhanced_state move player
;;

let get_enhanced_moves enhanced_state player =
  Enhanced_game_state.get_all_moves enhanced_state player
;;

let are_players_stuck enhanced_state =
  Enhanced_game_state.are_both_players_stuck enhanced_state
;;

let refresh_cards enhanced_state = Enhanced_game_state.refresh_center_cards enhanced_state
let check_win enhanced_state = Enhanced_game_state.check_for_immediate_win enhanced_state
let ai_move enhanced_state = Enhanced_game_state.ai_choose_move enhanced_state
let enhanced_to_string enhanced_state = Enhanced_game_state.to_string enhanced_state

(* Example values for testing *)
let initial_state = Game_state.create ()

let example_move =
  Move.Play_card { card = { Card.suit = Card.Hearts; rank = Card.Ace }; pile = 0 }
;;

let example_draw_move = Move.Draw_cards

let test_game () =
  Printf.printf "Testing HW2 Enhanced Game Logic...\n";
  let state = Enhanced_game_state.create () in
  Printf.printf "Created enhanced game state\n";
  Printf.printf "Game log: %s\n" (String.concat ~sep:"; " state.game_log)
;;

let () =
  Printf.printf "HW2 Enhanced Speed Card Game Logic loaded.\n";
  Printf.printf "Features: Simultaneous play, enhanced logging, stuck detection\n"
;;
