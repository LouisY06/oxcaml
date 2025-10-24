open! Core
open Hw1
open Hw2_speed_logic

(* HW4: Alpha-Beta Search for Speed Card Game *)
(* This module implements AI strategy using alpha-beta pruning for the Speed card game *)

module Card = Hw1.Card
module Player = Hw1.Player
module Move = Hw1.Move
module Game_state = Hw1.Game_state
(* module Enhanced_game_state = Hw2_speed_logic.Enhanced_game_state *)

(* Heuristic function for Speed card game *)
let heuristic_value (node : Game_state.t) =
  match node.winner with
  | Some Player.Player1 -> 1000  (* Player1 wins *)
  | Some Player.Player2 -> -1000 (* Player2 wins *)
  | None ->
    if node.game_over then 0 (* Game over but no winner (shouldn't happen in Speed) *)
    else
      (* Heuristic: prefer states where current player has fewer cards *)
      let current_hand_size = 
        match node.current_player with
        | Player.Player1 -> List.length node.player1_hand + List.length node.player1_stock
        | Player.Player2 -> List.length node.player2_hand + List.length node.player2_stock
      in
      let opponent_hand_size = 
        match node.current_player with
        | Player.Player1 -> List.length node.player2_hand + List.length node.player2_stock
        | Player.Player2 -> List.length node.player1_hand + List.length node.player1_stock
      in
      (* Positive score if current player has fewer cards (closer to winning) *)
      opponent_hand_size - current_hand_size

(* Enhanced heuristic that considers card playability *)
let enhanced_heuristic_value (node : Game_state.t) =
  match node.winner with
  | Some Player.Player1 -> 1000
  | Some Player.Player2 -> -1000
  | None ->
    if node.game_over then 0
    else
      let current_hand_size = 
        match node.current_player with
        | Player.Player1 -> List.length node.player1_hand + List.length node.player1_stock
        | Player.Player2 -> List.length node.player2_hand + List.length node.player2_stock
      in
      let opponent_hand_size = 
        match node.current_player with
        | Player.Player1 -> List.length node.player2_hand + List.length node.player2_stock
        | Player.Player2 -> List.length node.player1_hand + List.length node.player1_stock
      in
      
      (* Count playable cards for current player *)
      let current_hand = 
        match node.current_player with
        | Player.Player1 -> node.player1_hand
        | Player.Player2 -> node.player2_hand
      in
      let playable_cards = List.count current_hand ~f:(fun card ->
        Card.can_play_on card node.pile1 ||
        Card.can_play_on card node.pile2
      ) in
      
      (* Bonus for having more playable cards *)
      let playability_bonus = playable_cards * 10 in
      
      (* Base score: fewer cards is better *)
      let base_score = opponent_hand_size - current_hand_size in
      
      base_score + playability_bonus

let children node =
  let moves = Game_state.get_all_moves node in
  List.filter_map moves ~f:(fun move -> Game_state.make_move node move |> Result.ok)

(* Alpha-beta pruning algorithm for Speed card game *)
let rec alpha_beta (node : Game_state.t) depth alpha beta =
  if depth = 0 || node.game_over then
    enhanced_heuristic_value node
  else
    let children_nodes = children node in
    match node.current_player with
    | Player.Player1 ->
      (* Maximizing player *)
      List.fold_until
        children_nodes
        ~init:(Int.min_value, alpha)
        ~finish:(fun (value, _alpha) -> value)
        ~f:(fun (value, alpha) child ->
          let child_value = alpha_beta child (depth - 1) alpha beta in
          let new_value = Int.max value child_value in
          let new_alpha = Int.max alpha new_value in
          if new_value >= beta then Stop new_value else Continue (new_value, new_alpha))
    | Player.Player2 ->
      (* Minimizing player *)
      List.fold_until
        children_nodes
        ~init:(Int.max_value, beta)
        ~finish:(fun (value, _beta) -> value)
        ~f:(fun (value, beta) child ->
          let child_value = alpha_beta child (depth - 1) alpha beta in
          let new_value = Int.min value child_value in
          let new_beta = Int.min beta new_value in
          if new_value <= alpha then Stop new_value else Continue (new_value, new_beta))

(* Main alpha-beta search function *)
let alpha_beta_search (node : Game_state.t) ~depth =
  match node.winner with
  | Some _ -> None (* Game already over *)
  | None ->
    if node.game_over then None
    else
      let moves = Game_state.get_all_moves node in
      if List.is_empty moves then None
      else
        let moves_and_children =
          List.filter_map moves ~f:(fun move ->
            Game_state.make_move node move
            |> Result.ok
            |> Option.map ~f:(fun child -> (move, child)))
        in
        if List.is_empty moves_and_children then None
        else
          let moves_and_values =
            List.map moves_and_children ~f:(fun (move, child) ->
              let value = alpha_beta child (depth - 1) Int.min_value Int.max_value in
              (move, value))
          in
          let best_move =
            match node.current_player with
            | Player.Player1 ->
              (* Maximizing player - choose move with highest value *)
              List.max_elt moves_and_values ~compare:(fun (_, v1) (_, v2) -> Int.compare v1 v2)
              |> Option.map ~f:fst
            | Player.Player2 ->
              (* Minimizing player - choose move with lowest value *)
              List.min_elt moves_and_values ~compare:(fun (_, v1) (_, v2) -> Int.compare v1 v2)
              |> Option.map ~f:fst
          in
          best_move

(* Enhanced AI strategy for simultaneous play *)
module AIStrategy = struct
  (* Simple greedy strategy *)
  let greedy_strategy enhanced_state =
    let available_moves = Game_state.get_all_moves enhanced_state in
    if List.is_empty available_moves then
      None
    else
      (* Prefer playing cards over drawing *)
      let play_moves = List.filter available_moves ~f:(function
        | Move.Play_card _ -> true
        | Move.Draw_cards -> false
      ) in
      if not (List.is_empty play_moves) then
        Some (List.hd_exn play_moves)
      else
        Some (List.hd_exn available_moves)

  (* Alpha-beta strategy *)
  let alpha_beta_strategy enhanced_state ~depth =
    let available_moves = Game_state.get_all_moves enhanced_state in
    if List.is_empty available_moves then
      None
    else
      match alpha_beta_search enhanced_state ~depth with
      | Some move -> Some move
      | None -> greedy_strategy enhanced_state

  (* Hybrid strategy: use alpha-beta for important decisions, greedy for speed *)
  let hybrid_strategy enhanced_state =
    let available_moves = Game_state.get_all_moves enhanced_state in
    if List.is_empty available_moves then
      None
    else
      (* Use alpha-beta for deeper analysis when few moves available *)
      if List.length available_moves <= 3 then
        alpha_beta_strategy enhanced_state ~depth:3
      else
        greedy_strategy enhanced_state

  (* Adaptive strategy based on game state *)
  let adaptive_strategy enhanced_state =
    let available_moves = Game_state.get_all_moves enhanced_state in
    if List.is_empty available_moves then
      None
    else
      let player2_cards = List.length enhanced_state.player2_hand + 
                         List.length enhanced_state.player2_stock in
      let player1_cards = List.length enhanced_state.player1_hand + 
                         List.length enhanced_state.player1_stock in
      
      (* If we're behind, use deeper search *)
      if player2_cards > player1_cards + 5 then
        alpha_beta_strategy enhanced_state ~depth:4
      (* If we're ahead, use faster greedy *)
      else if player2_cards < player1_cards - 5 then
        greedy_strategy enhanced_state
      (* Otherwise, use moderate search *)
      else
        alpha_beta_strategy enhanced_state ~depth:2
end

(* AI move selection with different strategies *)
let ai_choose_move enhanced_state ~strategy =
  match strategy with
  | `Greedy -> AIStrategy.greedy_strategy enhanced_state
  | `AlphaBeta depth -> AIStrategy.alpha_beta_strategy enhanced_state ~depth
  | `Hybrid -> AIStrategy.hybrid_strategy enhanced_state
  | `Adaptive -> AIStrategy.adaptive_strategy enhanced_state

(* Default AI strategy *)
let default_ai_strategy = `Hybrid

(* Export functions for JavaScript integration *)
let ai_move enhanced_state = 
  ai_choose_move enhanced_state ~strategy:default_ai_strategy

let ai_move_with_strategy enhanced_state strategy =
  ai_choose_move enhanced_state ~strategy

(* Test functions for AI strategies *)
let test_ai_strategies () =
  let enhanced_state = Game_state.create () in
  let strategies = [`Greedy; `AlphaBeta 2; `Hybrid; `Adaptive] in
  
  List.iter strategies ~f:(fun strategy ->
    match ai_choose_move enhanced_state ~strategy with
    | Some move -> 
        Stdio.printf "Strategy %s chose move: %s\n" 
          (Sexp.to_string (sexp_of_string (match strategy with
            | `Greedy -> "Greedy"
            | `AlphaBeta _ -> "AlphaBeta"
            | `Hybrid -> "Hybrid"
            | `Adaptive -> "Adaptive")))
          (Sexp.to_string (Move.sexp_of_t move))
    | None -> 
        Stdio.printf "Strategy %s found no moves\n" 
          (match strategy with
            | `Greedy -> "Greedy"
            | `AlphaBeta _ -> "AlphaBeta"
            | `Hybrid -> "Hybrid"
            | `Adaptive -> "Adaptive");
  )

let () = 
  Stdio.printf "HW4 Alpha-Beta Search for Speed Card Game loaded.\n";
  Stdio.printf "Available AI strategies: Greedy, AlphaBeta, Hybrid, Adaptive\n"