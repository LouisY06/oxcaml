open! Core
open! Base
open Speed_logic_library
open! Bonsai
open! Bonsai.Let_syntax
open! Bonsai_web

(* HW6: Speed Card Game UI using Bonsai *)
(* This module maps the HW2 game logic to HTML + CSS using Bonsai's reactive framework *)

module Model = struct
   type t =
      { enhanced_state : Hw2_speed_logic.Enhanced_game_state.t
      ; selected_card : Hw2_speed_logic.Card.t option
      ; ai_thinking : bool
      ; game_message : string
  }
  [@@deriving sexp, compare, equal]

   let initial =
      { enhanced_state = Hw2_speed_logic.Enhanced_game_state.create ()
      ; selected_card = None
      ; ai_thinking = false
      ; game_message = "Click on your card, then click on a center pile to play!"
      }
   ;;
end

module Action = struct
  type t =
    | New_game
      | Select_card of Hw2_speed_logic.Card.t
      | Play_on_pile of int (* pile index 0 or 1 *)
    | AI_move
      | Auto_draw_player1
      | Auto_draw_player2
      | Check_stuck
  [@@deriving sexp, compare]
end

let apply_action (action : Action.t) (model : Model.t) : Model.t =
  match action with
  | New_game ->
      let new_enhanced_state = Hw2_speed_logic.Enhanced_game_state.create () in
      { enhanced_state = new_enhanced_state
      ; selected_card = None
      ; ai_thinking = false
      ; game_message = "New game started! Click on your card, then click on a center pile to play!"
    }
  
  | Select_card card ->
      if model.enhanced_state.base_state.game_over then
         model
      else
         { model with 
           selected_card = Some card
         ; game_message = "Card selected! Now click on a center pile to play it."
         }
   
   | Play_on_pile pile_index ->
      if model.enhanced_state.base_state.game_over then
         model
      else
         (match model.selected_card with
          | None -> 
             { model with game_message = "Select a card first!" }
          | Some card ->
             let player_id = "Player1" in
             let move = Hw2_speed_logic.Move.Play_card { card; pile = pile_index } in
             (match Hw2_speed_logic.Enhanced_game_state.make_move model.enhanced_state move player_id with
              | Ok new_enhanced_state ->
                 if new_enhanced_state.base_state.game_over then
                   (match new_enhanced_state.base_state.winner with
                    | Some Hw2_speed_logic.Player.Player1 -> 
                       { enhanced_state = new_enhanced_state
                       ; selected_card = None
                       ; ai_thinking = false
                       ; game_message = "🎉 YOU WIN! 🎉 Click 'New Game' to play again."
                       }
                    | Some Hw2_speed_logic.Player.Player2 ->
                       { enhanced_state = new_enhanced_state
                       ; selected_card = None
                       ; ai_thinking = false
                       ; game_message = "😞 AI WINS! 😞 Click 'New Game' to play again."
                       }
                    | None ->
                       { enhanced_state = new_enhanced_state
                       ; selected_card = None
                       ; ai_thinking = false
                       ; game_message = "Game Over! Click 'New Game' to play again."
                       })
                 else
                   { enhanced_state = new_enhanced_state
                   ; selected_card = None
                   ; ai_thinking = false
                   ; game_message = "Good move! AI is thinking..."
                   }
              | Error msg -> 
      { model with 
                   game_message = "Invalid move: " ^ msg ^ " Try another pile!"
                 }))
  
   | AI_move ->
      if model.enhanced_state.base_state.game_over || model.ai_thinking then
         model
      else
         (match Hw2_speed_logic.Enhanced_game_state.ai_choose_move model.enhanced_state with
          | Some ai_move ->
             let player_id = "Player2" in
             (match Hw2_speed_logic.Enhanced_game_state.make_move model.enhanced_state ai_move player_id with
              | Ok new_enhanced_state ->
                 if new_enhanced_state.base_state.game_over then
                   (match new_enhanced_state.base_state.winner with
                    | Some Hw2_speed_logic.Player.Player1 -> 
                       { enhanced_state = new_enhanced_state
                       ; selected_card = None
                       ; ai_thinking = false
                       ; game_message = "🎉 YOU WIN! 🎉 Click 'New Game' to play again."
                       }
                    | Some Hw2_speed_logic.Player.Player2 ->
                       { enhanced_state = new_enhanced_state
                       ; selected_card = None
                       ; ai_thinking = false
                       ; game_message = "😞 AI WINS! 😞 Click 'New Game' to play again."
                       }
                    | None ->
                       { enhanced_state = new_enhanced_state
                       ; selected_card = None
                       ; ai_thinking = false
                       ; game_message = "Game Over! Click 'New Game' to play again."
                       })
                 else
                   { enhanced_state = new_enhanced_state
                   ; selected_card = None
                   ; ai_thinking = false
                   ; game_message = "AI played! Your turn."
                   }
              | Error _msg -> 
                 { model with ai_thinking = false })
          | None -> 
             { model with 
               ai_thinking = false
             ; game_message = "AI has no valid moves. Your turn!"
             })
   
   | Auto_draw_player1 ->
      if model.enhanced_state.base_state.game_over then
         model
      else if List.length model.enhanced_state.base_state.player1_hand < 5 
              && not (List.is_empty model.enhanced_state.base_state.player1_stock) then
         let move = Hw2_speed_logic.Move.Draw_cards in
         (match Hw2_speed_logic.Enhanced_game_state.make_move model.enhanced_state move "Player1" with
          | Ok new_enhanced_state -> { model with enhanced_state = new_enhanced_state }
          | Error _ -> model)
      else
         model
   
   | Auto_draw_player2 ->
      if model.enhanced_state.base_state.game_over then
         model
      else if List.length model.enhanced_state.base_state.player2_hand < 5 
              && not (List.is_empty model.enhanced_state.base_state.player2_stock) then
         let move = Hw2_speed_logic.Move.Draw_cards in
         (match Hw2_speed_logic.Enhanced_game_state.make_move model.enhanced_state move "Player2" with
          | Ok new_enhanced_state -> { model with enhanced_state = new_enhanced_state }
          | Error _ -> model)
      else
         model
   
   | Check_stuck ->
      if model.enhanced_state.base_state.game_over then
         model
      else if Hw2_speed_logic.Enhanced_game_state.are_both_players_stuck model.enhanced_state then
         let new_enhanced_state = Hw2_speed_logic.Enhanced_game_state.refresh_center_cards model.enhanced_state in
         { model with 
           enhanced_state = new_enhanced_state
         ; game_message = "Both players stuck! Center cards refreshed."
         }
      else
         model
;;

(* Bonsai components for mapping game logic to HTML + CSS *)
module Components = struct
   open Bonsai_web.Vdom
   open Attr

   (* Helper to render a card *)
   let card_to_html card is_selected is_player_card ~inject =
      let suit_symbol = match card.Hw2_speed_logic.Card.suit with
         | Hw2_speed_logic.Card.Hearts -> "♥"
         | Hw2_speed_logic.Card.Diamonds -> "♦"
         | Hw2_speed_logic.Card.Clubs -> "♣"
         | Hw2_speed_logic.Card.Spades -> "♠"
      in
      let rank_str = match card.Hw2_speed_logic.Card.rank with
         | Hw2_speed_logic.Card.Ace -> "A"
         | Hw2_speed_logic.Card.Two -> "2"
         | Hw2_speed_logic.Card.Three -> "3"
         | Hw2_speed_logic.Card.Four -> "4"
         | Hw2_speed_logic.Card.Five -> "5"
         | Hw2_speed_logic.Card.Six -> "6"
         | Hw2_speed_logic.Card.Seven -> "7"
         | Hw2_speed_logic.Card.Eight -> "8"
         | Hw2_speed_logic.Card.Nine -> "9"
         | Hw2_speed_logic.Card.Ten -> "10"
         | Hw2_speed_logic.Card.Jack -> "J"
         | Hw2_speed_logic.Card.Queen -> "Q"
         | Hw2_speed_logic.Card.King -> "K"
      in
      let suit_color = match card.Hw2_speed_logic.Card.suit with
         | Hw2_speed_logic.Card.Hearts | Hw2_speed_logic.Card.Diamonds -> "red"
         | Hw2_speed_logic.Card.Clubs | Hw2_speed_logic.Card.Spades -> "black"
      in
      
      let classes = ["card"] in
      let classes = if is_selected then "selected" :: classes else classes in
      let classes = if not is_player_card then "face-down" :: classes else classes in
      
      Node.div
         ~attrs:
            [ Attr.create "class" (String.concat ~sep:" " classes)
            ; (if is_player_card then on_click (fun _ -> inject (Action.Select_card card)) else Attr.empty)
            ; Attr.create "style" ("color: " ^ suit_color ^ "; cursor: " ^ (if is_player_card then "pointer" else "default"))
            ]
         [ Node.text (if is_player_card then rank_str ^ suit_symbol else "🂠") ]

   let view (model : Model.t) (inject : Action.t -> unit Effect.t) =
      let open Hw2_speed_logic in
      let open Vdom in

      (* Player hand *)
      let player_hand_html =
         Node.div
            ~attrs:[ Attr.create "class" "hand"; Attr.create "id" "player1Hand" ]
            (List.map model.enhanced_state.base_state.player1_hand ~f:(fun card ->
                 card_to_html card
                    (Option.equal Card.equal model.selected_card (Some card))
                    true ~inject))
      in

      (* AI hand (face down) *)
      let ai_hand_html =
         Node.div
            ~attrs:[ Attr.create "class" "hand"; Attr.create "id" "aiHand" ]
            (List.map model.enhanced_state.base_state.player2_hand ~f:(fun card ->
                 card_to_html card false false ~inject))
      in

      (* Center piles *)
      let pile_html pile_id pile_index pile_card_opt =
         let can_play = Option.is_some model.selected_card in
         Node.div
            ~attrs:
               [ Attr.create "class" ("pile" ^ (if can_play then " pile-active" else ""))
               ; Attr.create "id" pile_id
               ; Attr.create "style" ("cursor: " ^ (if can_play then "pointer" else "default"))
               ; on_click (fun _ -> if can_play then inject (Action.Play_on_pile pile_index) else Effect.Ignore)
               ]
            [ Node.div ~attrs:[ Attr.create "class" "pile-label" ]
                 [ Node.text (Printf.sprintf "Pile %d" (pile_index + 1)) ]
            ; (match pile_card_opt with
               | Some card -> card_to_html card false true ~inject
               | None -> 
                  Node.div 
                     ~attrs:[ Attr.create "class" "card empty-pile" ] 
                     [ Node.text "Empty" ])
            ]
      in

      let pile1_html = pile_html "pile1" 0 model.enhanced_state.base_state.pile1 in
      let pile2_html = pile_html "pile2" 1 model.enhanced_state.base_state.pile2 in

      (* Game log *)
      let game_log_html =
         Node.div
            ~attrs:[ Attr.create "class" "game-log"; Attr.create "id" "gameLog" ]
            (List.take (List.rev model.enhanced_state.game_log) 10 
             |> List.map ~f:(fun msg ->
                  Node.div ~attrs:[ Attr.create "class" "log-entry" ] [ Node.text msg ]))
      in

      Node.div
         ~attrs:[ Attr.create "class" "game-container" ]
         [ Node.div
              ~attrs:[ Attr.create "class" "game-header" ]
              [ Node.h1 [ Node.text "HW6: Speed Card Game (OCaml + Bonsai)" ]
              ; Node.div ~attrs:[ Attr.create "class" "game-status"; Attr.create "id" "gameStatus" ]
                   [ Node.text model.game_message ]
              ; Node.div
                   ~attrs:[ Attr.create "class" "game-controls" ]
                   [ Node.button ~attrs:[ on_click (fun _ -> inject Action.New_game) ]
                        [ Node.text "New Game" ]
                   ; Node.button ~attrs:[ on_click (fun _ -> inject Action.AI_move) ]
                        [ Node.text "Trigger AI Move" ]
                   ; Node.button ~attrs:[ on_click (fun _ -> inject Action.Auto_draw_player1) ]
                        [ Node.text "Draw Card" ]
                   ; Node.button ~attrs:[ on_click (fun _ -> inject Action.Check_stuck) ]
                        [ Node.text "Check if Stuck" ]
                   ]
              ]
         ; Node.div
              ~attrs:[ Attr.create "class" "game-board" ]
              [ Node.div ~attrs:[ Attr.create "class" "player-area player2-area" ]
                   [ Node.div ~attrs:[ Attr.create "class" "player-label" ] [ Node.text "AI Player" ]
                   ; ai_hand_html
                   ; Node.div ~attrs:[ Attr.create "class" "stock-pile" ]
                        [ Node.div ~attrs:[ Attr.create "class" "stock-label" ]
                             [ Node.text
                                  (Printf.sprintf "Draw Pile: %d cards"
                                     (List.length model.enhanced_state.base_state.player2_stock))
                             ]
                        ; Node.div ~attrs:[ Attr.create "class" "card face-down stock" ]
                             [ Node.text "🂠" ]
                        ]
                   ]
              ; Node.div ~attrs:[ Attr.create "class" "center-area" ]
                   [ Node.div ~attrs:[ Attr.create "class" "pile-area" ] [ pile1_html; pile2_html ] ]
              ; Node.div ~attrs:[ Attr.create "class" "player-area player1-area" ]
                   [ Node.div ~attrs:[ Attr.create "class" "player-label" ] [ Node.text "You (Player 1)" ]
                   ; player_hand_html
                   ; Node.div ~attrs:[ Attr.create "class" "stock-pile" ]
                        [ Node.div ~attrs:[ Attr.create "class" "stock-label" ]
                             [ Node.text
                                  (Printf.sprintf "Draw Pile: %d cards"
                                     (List.length model.enhanced_state.base_state.player1_stock))
                             ]
                        ; Node.div ~attrs:[ Attr.create "class" "card face-down stock" ]
                             [ Node.text "🂠" ]
                        ]
                   ]
              ]
         ; Node.div
              ~attrs:[ Attr.create "class" "game-info" ]
              [ Node.div ~attrs:[ Attr.create "class" "info-item" ]
                   [ Node.strong [ Node.text "How to Play: " ]
                   ; Node.text "Click on your card to select it (green border), then click on a center pile to play!"
                   ]
              ; Node.div ~attrs:[ Attr.create "class" "info-item" ]
                   [ Node.strong [ Node.text "Rules: " ]
                   ; Node.text "Cards must be ±1 rank from pile top. Aces can play on 2 or King."
                   ]
              ; Node.div ~attrs:[ Attr.create "class" "info-item" ]
                   [ Node.strong [ Node.text "Win Condition: " ]
                   ; Node.text "Empty your hand and stock pile (15 cards) before the AI!"
                   ]
              ; Node.div ~attrs:[ Attr.create "class" "info-item" ]
                   [ Node.strong [ Node.text "Game Log:" ] ]
              ; game_log_html
              ]
         ]
   ;;
end

let app =
   let%sub model, inject =
      Bonsai.state (module Model) ~default_model:Model.initial
   in
   let%arr model = model and inject = inject in
   let inject_action action = inject (apply_action action model) in
   Components.view model inject_action
