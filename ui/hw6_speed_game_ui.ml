open! Core
open! Base
open Speed_logic_library
open! Bonsai
open! Bonsai.Let_syntax
open! Bonsai_web

(* HW6: Speed Card Game UI using Bonsai *)
(* Simultaneous play - both players can play at any time! *)

module Model = struct
   type t =
      { enhanced_state : Hw2_speed_logic.Enhanced_game_state.t
      ; selected_card : Hw2_speed_logic.Card.t option
      ; game_message : string
      ; countdown : int option  (* None = no countdown, Some n = counting down from n *)
  }
  [@@deriving sexp, compare, equal]

   let initial =
      { enhanced_state = Hw2_speed_logic.Enhanced_game_state.create ()
      ; selected_card = None
      ; game_message = "Welcome! Click on your card, then click on a center pile to play!"
      ; countdown = None
      }
   ;;
end

module Action = struct
  type t =
    | New_game
    | Select_card of Hw2_speed_logic.Card.t
    | Play_on_pile of int (* pile index 0 or 1 *)
    | AI_move_continuous (* AI plays continuously *)
    | Trigger_periodic_update (* Periodic game update *)
    | Start_refresh_countdown
    | Countdown_tick
  [@@deriving sexp, compare]
end

(* Helper to auto-draw cards - keep drawing until hand has 5 cards *)
let auto_draw_until_full (enhanced_state : Hw2_speed_logic.Enhanced_game_state.t) (player_id : string) 
  : Hw2_speed_logic.Enhanced_game_state.t =
  let rec draw_loop (enh_state : Hw2_speed_logic.Enhanced_game_state.t) =
    let base = enh_state.base_state in
    let hand_size, stock_size = 
      if String.equal player_id "Player1" then
        (List.length base.player1_hand, List.length base.player1_stock)
      else
        (List.length base.player2_hand, List.length base.player2_stock)
    in
    if hand_size < 5 && stock_size > 0 then
      let move = Hw2_speed_logic.Move.Draw_cards in
      match Hw2_speed_logic.Enhanced_game_state.make_move enh_state move player_id with
      | Ok new_state -> draw_loop new_state  (* Keep drawing until hand is full *)
      | Error _ -> enh_state
    else
      enh_state
  in
  draw_loop enhanced_state
;;

(* Check if both players are stuck - returns (state, is_stuck, message) *)
let check_if_stuck (enhanced_state : Hw2_speed_logic.Enhanced_game_state.t) 
  : Hw2_speed_logic.Enhanced_game_state.t * bool * string =
  let is_stuck = Hw2_speed_logic.Enhanced_game_state.are_both_players_stuck enhanced_state in
  let () = Stdio.printf "\n=== HANDS CHECK ===\n" in
  let () = Stdio.printf "Player 1 hand: " in
  let () = List.iter enhanced_state.base_state.player1_hand ~f:(fun c ->
    Stdio.printf "%s " (Hw2_speed_logic.Card.to_string c)) in
  let () = Stdio.printf "\nPlayer 2 hand: " in
  let () = List.iter enhanced_state.base_state.player2_hand ~f:(fun c ->
    Stdio.printf "%s " (Hw2_speed_logic.Card.to_string c)) in
  let () = Stdio.printf "\nPile 1: %s | Pile 2: %s\n" 
    (match enhanced_state.base_state.pile1 with Some c -> Hw2_speed_logic.Card.to_string c | None -> "Empty")
    (match enhanced_state.base_state.pile2 with Some c -> Hw2_speed_logic.Card.to_string c | None -> "Empty") in
  let () = Stdio.printf "Both stuck? %b\n%!" is_stuck in
  (enhanced_state, is_stuck, if is_stuck then "Both players stuck!" else "")
;;

let apply_action (action : Action.t) (model : Model.t) : Model.t =
  match action with
  | New_game ->
      let new_enhanced_state = Hw2_speed_logic.Enhanced_game_state.create () in
      { enhanced_state = new_enhanced_state
      ; selected_card = None
      ; game_message = "New game! You have 5 cards, 15 in draw pile. Play fast!"
      ; countdown = None
    }
  
  | Select_card card ->
      if model.enhanced_state.base_state.game_over then
         model
      else
        { model with 
            selected_card = Some card
          ; game_message = "Card selected! Click on a center pile to play it."
          }
   
  | Play_on_pile pile_index ->
      if model.enhanced_state.base_state.game_over then
         model
      else
         (match model.selected_card with
          | None -> 
             { model with game_message = "Select a card from your hand first!" }
          | Some card ->
             let player_id = "Player1" in
             let move = Hw2_speed_logic.Move.Play_card { card; pile = pile_index } in
               (match Hw2_speed_logic.Enhanced_game_state.make_move model.enhanced_state move player_id with
              | Ok new_enhanced_state ->
                 let () = Stdio.printf "After play - P1: hand=%d stock=%d, P2: hand=%d stock=%d, game_over=%b\n%!"
                   (List.length new_enhanced_state.base_state.player1_hand)
                   (List.length new_enhanced_state.base_state.player1_stock)
                   (List.length new_enhanced_state.base_state.player2_hand)
                   (List.length new_enhanced_state.base_state.player2_stock)
                   new_enhanced_state.base_state.game_over in
                 (* Auto-draw after playing - fill hand back to 5 *)
                 let state_after_draw = auto_draw_until_full new_enhanced_state player_id in
                 let () = Stdio.printf "After draw - P1: hand=%d stock=%d, P2: hand=%d stock=%d, game_over=%b\n%!"
                   (List.length state_after_draw.base_state.player1_hand)
                   (List.length state_after_draw.base_state.player1_stock)
                   (List.length state_after_draw.base_state.player2_hand)
                   (List.length state_after_draw.base_state.player2_stock)
                   state_after_draw.base_state.game_over in
                 (* Check if stuck *)
                 let state_check, is_stuck, _ = check_if_stuck state_after_draw in
                 
                 (* If stuck, refresh the piles *)
                 let final_state = if is_stuck then
                   let () = Stdio.printf "🔄 REFRESHING! 3... 2... 1...\n%!" in
                   Hw2_speed_logic.Enhanced_game_state.refresh_center_cards state_check
                 else
                   state_check
                 in
                 
                 (* AI plays continuously via Clock.every - don't play AI moves here! *)
                 if final_state.base_state.game_over then
                   (match final_state.base_state.winner with
                    | Some Hw2_speed_logic.Player.Player1 -> 
                       { enhanced_state = final_state
                       ; selected_card = None
                       ; game_message = "YOU WIN! All cards played! Click 'New Game' to play again."
                       ; countdown = None
                       }
                    | Some Hw2_speed_logic.Player.Player2 ->
                       { enhanced_state = final_state
                       ; selected_card = None
                       ; game_message = "AI WINS! AI played all cards first. Click 'New Game' to try again."
                       ; countdown = None
                       }
                    | None ->
                       { enhanced_state = final_state
                       ; selected_card = None
                       ; game_message = "Game Over! Click 'New Game' to play again."
                       ; countdown = None
                       })
                 else if is_stuck then
                   { enhanced_state = final_state
                   ; selected_card = None
                   ; game_message = "REFRESHING! 3... 2... 1... GO!"
                   ; countdown = Some 3
                   }
                 else
                   { enhanced_state = final_state
                   ; selected_card = None
                   ; game_message = "Good play! Keep going!"
                   ; countdown = None
                   }
              | Error msg ->
                 { model with 
                   game_message = "Can't play there: " ^ msg ^ " Try the other pile!"
                 }))
   
  | AI_move_continuous | Trigger_periodic_update ->
      if model.enhanced_state.base_state.game_over then
         model
      else
         (* Only auto-draw for AI, NOT for Player1! Player1 draws after playing. *)
         let state_with_draws = auto_draw_until_full model.enhanced_state "Player2" in
         
         (* Let AI try to play multiple cards in a burst *)
         let rec ai_play_all (enh_state : Hw2_speed_logic.Enhanced_game_state.t) max_moves =
           if max_moves <= 0 || enh_state.base_state.game_over then
             enh_state
           else
             match Hw2_speed_logic.Enhanced_game_state.ai_choose_move enh_state with
             | Some ai_move ->
                (match Hw2_speed_logic.Enhanced_game_state.make_move enh_state ai_move "Player2" with
                 | Ok new_state ->
                    let state_with_draw = auto_draw_until_full new_state "Player2" in
                    let state_check, is_stuck, _ = check_if_stuck state_with_draw in
                    let state_after_stuck = if is_stuck then
                      Hw2_speed_logic.Enhanced_game_state.refresh_center_cards state_check
                    else
                      state_check
                    in
                    ai_play_all state_after_stuck (max_moves - 1)
                 | Error _ -> enh_state)
             | None -> enh_state
         in
         
         let final_state = ai_play_all state_with_draws 1 in
         let () = Stdio.printf "AI update - P1: hand=%d stock=%d, P2: hand=%d stock=%d, game_over=%b\n%!"
           (List.length final_state.base_state.player1_hand)
           (List.length final_state.base_state.player1_stock)
           (List.length final_state.base_state.player2_hand)
           (List.length final_state.base_state.player2_stock)
           final_state.base_state.game_over in
         
         if final_state.base_state.game_over then
           (let () = Stdio.printf "🏆 GAME OVER! Winner: %s\n%!"
             (match final_state.base_state.winner with
              | Some Hw2_speed_logic.Player.Player1 -> "Player 1"
              | Some Hw2_speed_logic.Player.Player2 -> "Player 2"
              | None -> "None") in
            match final_state.base_state.winner with
            | Some Hw2_speed_logic.Player.Player1 -> 
              { enhanced_state = final_state
              ; selected_card = None
              ; game_message = "YOU WIN! All cards played!"
              ; countdown = None
              }
            | Some Hw2_speed_logic.Player.Player2 ->
              { enhanced_state = final_state
              ; selected_card = None
              ; game_message = "AI WINS! AI was too fast!"
              ; countdown = None
              }
            | None ->
               { enhanced_state = final_state
               ; selected_card = None
               ; game_message = "Game Over!"
               ; countdown = None
               })
         else
           { enhanced_state = final_state
           ; selected_card = model.selected_card
           ; game_message = model.game_message
           ; countdown = model.countdown
           }
  
  | Start_refresh_countdown | Countdown_tick ->
      (* Not used anymore, but keep for compatibility *)
      model
;;

(* Bonsai components for mapping game logic to HTML + CSS *)
module Components = struct
   open Bonsai_web.Vdom
   open Attr

   (* Helper to render a card *)
   let card_to_html card is_selected is_player_card is_face_down ~inject =
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
      
      let base_classes = ["card"] in
      let classes = if is_selected then "selected" :: base_classes else base_classes in
      let classes = if is_face_down then "face-down" :: classes else classes in
      
      Node.div
         ~attrs:
            [ Attr.create "class" (String.concat ~sep:" " classes)
            ; (if is_player_card then on_click (fun _ -> inject (Action.Select_card card)) else Attr.empty)
            ; Attr.create "style" ("color: " ^ suit_color ^ "; cursor: " ^ (if is_player_card then "pointer" else "default"))
            ]
         [ Node.text (if is_face_down then "?" else rank_str ^ suit_symbol) ]

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
                    true false ~inject))
      in

      (* AI hand (face down) *)
      let ai_hand_html =
         Node.div
            ~attrs:[ Attr.create "class" "hand"; Attr.create "id" "aiHand" ]
            (List.map model.enhanced_state.base_state.player2_hand ~f:(fun card ->
                 card_to_html card false false true ~inject))
      in

      (* Center piles *)
      let pile_html pile_id pile_index pile_card_opt =
         let can_play = Option.is_some model.selected_card && not model.enhanced_state.base_state.game_over in
         Node.div
            ~attrs:
               [ Attr.create "class" ("pile" ^ (if can_play then " pile-active" else ""))
               ; Attr.create "id" pile_id
               ; Attr.create "style" ("cursor: " ^ (if can_play then "pointer" else "default") ^ "; border: 3px solid " ^ (if can_play then "#4CAF50" else "#ddd"))
               ; on_click (fun _ -> if can_play then inject (Action.Play_on_pile pile_index) else Effect.Ignore)
               ]
            [ Node.div ~attrs:[ Attr.create "class" "pile-label" ]
                 [ Node.text (Printf.sprintf "Pile %d" (pile_index + 1)) ]
            ; (match pile_card_opt with
               | Some card -> 
                  (* Render pile card - SHOW THE CARD but make it unclickable! *)
                  let pile_card_html = card_to_html card false false false ~inject in
                  Node.div 
                     ~attrs:[ Attr.create "style" "pointer-events: none;" ]
                     [ pile_card_html ]
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
              [ Node.h1 [ Node.text "Speed Card Game" ]
              ; Node.div ~attrs:[ Attr.create "class" "game-status"; Attr.create "id" "gameStatus" ]
                   [ Node.text model.game_message ]
              ; Node.div
                   ~attrs:[ Attr.create "class" "game-controls" ]
                   [ Node.button 
                        ~attrs:[ on_click (fun _ -> inject Action.New_game)
                               ; Attr.create "style" "padding: 10px 20px; border: 2px solid black; cursor: pointer; border-radius: 5px; font-size: 16px; background: white;"
                               ] 
                        [ Node.text "New Game" ]
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
                             [ Node.text "?" ]
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
                             [ Node.text "?" ]
                        ]
                   ]
              ]
         ; Node.div
              ~attrs:[ Attr.create "class" "game-info" ]
              [ Node.div ~attrs:[ Attr.create "class" "info-item" ]
                   [ Node.strong [ Node.text "⚡ SPEED MODE: " ]
                   ; Node.text "Both players play simultaneously! No turns! Play as fast as you can!"
                   ]
              ; Node.div ~attrs:[ Attr.create "class" "info-item" ]
                   [ Node.strong [ Node.text "🎮 How to Play: " ]
                   ; Node.text "Click your card → Click center pile. Cards auto-draw after playing!"
                   ]
              ; Node.div ~attrs:[ Attr.create "class" "info-item" ]
                   [ Node.strong [ Node.text "📋 Rules: " ]
                   ; Node.text "Play cards ±1 rank from pile top. Aces play on 2 or King."
                   ]
              ; Node.div ~attrs:[ Attr.create "class" "info-item" ]
                   [ Node.strong [ Node.text "🏆 Win: " ]
                   ; Node.text "Empty all 20 cards (5 in hand + 15 in draw pile) before the AI!"
                   ]
              ; Node.div ~attrs:[ Attr.create "class" "info-item" ]
                   [ Node.strong [ Node.text "📜 Game Log:" ] ]
              ; game_log_html
              ]
         ]
   ;;
end

(* ================================= *)
(* 🚀 FIXED Bonsai App Initialization *)
(* ================================= *)
let app =
  let%sub model, inject =
    Bonsai.state_machine0
      (module Model)
      (module Action)
      ~default_model:Model.initial
      ~apply_action:(fun ~inject:_ ~schedule_event:_ _model action -> apply_action action _model)
  in

  (* Periodic AI update every 2000ms (2 seconds) - AI plays 1 card every 2 seconds *)
  let%sub () =
    Bonsai.Clock.every
      ~when_to_start_next_effect:`Every_multiple_of_period_blocking
      ~trigger_on_activate:true
      (Time_ns.Span.of_ms 2000.0)
      (let%map inject = inject in
       inject Action.Trigger_periodic_update)
  in

  let%arr model = model
  and inject = inject in
  let inject_action action = inject action in
  Components.view model inject_action
;;
