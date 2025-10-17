open! Core
open Hw2_speed_logic

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

let children node =
  let moves = Game_state.get_all_moves node in
  List.filter_map moves ~f:(fun move -> Game_state.make_move node move |> Result.ok)

(* Alpha-beta pruning algorithm for Speed card game *)
let rec alpha_beta (node : Game_state.t) depth alpha beta =
  if depth = 0 || node.game_over then
    heuristic_value node
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
let alpha_beta (node : Game_state.t) ~depth =
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