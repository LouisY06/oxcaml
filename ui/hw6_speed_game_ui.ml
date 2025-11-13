open! Core
open! Base
open Speed_logic_library
open! Bonsai
open! Bonsai.Let_syntax
open! Bonsai_web
open Js_of_ocaml

(* HW6: Speed Card Game UI using Bonsai *)
(* Simultaneous play - both players can play at any time! *)
(* Offline support with Service Worker and Local Storage *)

module Model = struct
   type t =
      { enhanced_state : Hw2_speed_logic.Enhanced_game_state.t
      ; selected_card : Hw2_speed_logic.Card.t option
      ; game_message : string
  }
  [@@deriving sexp, compare, equal]

   let initial =
      { enhanced_state = Hw2_speed_logic.Enhanced_game_state.create ()
      ; selected_card = None
      ; game_message = "Welcome! Click on your card, then click on a center pile to play!"
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
    | Load_saved_game (* Load game from local storage *)
  [@@deriving sexp, compare]
end

(* Local Storage helpers for saving/loading game state *)
module LocalStorage = struct
  let storage_key = "speed_game_state"
  
  let save (model : Model.t) : unit =
    try
      let sexp = Model.sexp_of_t model in
      let json_str = Sexp.to_string sexp in
      match Js.Optdef.to_option Dom_html.window##.localStorage with
      | None -> ()
      | Some storage ->
        let key = Js.string storage_key in
        let value = Js.string json_str in
        storage##setItem key value;
      (* Show visual save indicator *)
      (try
         let show_indicator = Js.Unsafe.global##.showSaveIndicator in
         if Js.Opt.test show_indicator then
           (Js.Unsafe.fun_call show_indicator [||])
       with _ -> ());
      let () = Stdio.printf "Game saved to local storage\n%!" in
      ()
    with
    | _ -> 
      let () = Stdio.printf "Failed to save game to local storage\n%!" in
      ()
  
  let load () : Model.t option =
    try
      match Js.Optdef.to_option Dom_html.window##.localStorage with
      | None -> None
      | Some storage ->
        let key = Js.string storage_key in
        match Js.Opt.to_option (storage##getItem key) with
        | None -> None
        | Some value ->
          let json_str = Js.to_string value in
          let sexp = Parsexp.Single.parse_string_exn json_str in
          let model = Model.t_of_sexp sexp in
          let () = Stdio.printf "Game loaded from local storage\n%!" in
          Some model
    with
    | _ -> 
      let () = Stdio.printf "Failed to load game from local storage\n%!" in
      None
  
  let clear () : unit =
    try
      match Js.Optdef.to_option Dom_html.window##.localStorage with
      | None -> ()
      | Some storage ->
        let key = Js.string storage_key in
        storage##removeItem key;
      let () = Stdio.printf "Local storage cleared\n%!" in
      ()
    with
    | _ -> ()
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

(* Check if both players are stuck and keep refreshing until someone can play *)
let check_and_refresh_if_stuck (enhanced_state : Hw2_speed_logic.Enhanced_game_state.t) 
  : Hw2_speed_logic.Enhanced_game_state.t * string =
  let pile1_card = enhanced_state.base_state.pile1 in
  let pile2_card = enhanced_state.base_state.pile2 in
  
  (* Check each player individually *)
  let player1_can_play =
    List.exists enhanced_state.base_state.player1_hand ~f:(fun card ->
      Hw2_speed_logic.Card.can_play_on card pile1_card
      || Hw2_speed_logic.Card.can_play_on card pile2_card)
  in
  let player2_can_play =
    List.exists enhanced_state.base_state.player2_hand ~f:(fun card ->
      Hw2_speed_logic.Card.can_play_on card pile1_card
      || Hw2_speed_logic.Card.can_play_on card pile2_card)
  in
  
  let is_stuck = (not player1_can_play) && (not player2_can_play) in
  
  let () = Stdio.printf "\n=== STUCK CHECK ===\n" in
  let () = Stdio.printf "Player 1 hand: " in
  let () = List.iter enhanced_state.base_state.player1_hand ~f:(fun c ->
    Stdio.printf "%s " (Hw2_speed_logic.Card.to_string c)) in
  let () = Stdio.printf " (can play: %b)\n" player1_can_play in
  let () = Stdio.printf "Player 2 hand: " in
  let () = List.iter enhanced_state.base_state.player2_hand ~f:(fun c ->
    Stdio.printf "%s " (Hw2_speed_logic.Card.to_string c)) in
  let () = Stdio.printf " (can play: %b)\n" player2_can_play in
  let () = Stdio.printf "Pile 1: %s | Pile 2: %s\n" 
    (match pile1_card with Some c -> Hw2_speed_logic.Card.to_string c | None -> "Empty")
    (match pile2_card with Some c -> Hw2_speed_logic.Card.to_string c | None -> "Empty") in
  let () = Stdio.printf "Both stuck? %b\n%!" is_stuck in
  if is_stuck then
    (* Keep refreshing until at least one player can play *)
    let rec refresh_until_playable state refresh_count =
      let new_state = Hw2_speed_logic.Enhanced_game_state.refresh_center_cards state in
      let () = Stdio.printf "REFRESH #%d! New Pile 1: %s | New Pile 2: %s\n%!"
        refresh_count
        (match new_state.base_state.pile1 with Some c -> Hw2_speed_logic.Card.to_string c | None -> "Empty")
        (match new_state.base_state.pile2 with Some c -> Hw2_speed_logic.Card.to_string c | None -> "Empty") in
      let still_stuck = Hw2_speed_logic.Enhanced_game_state.are_both_players_stuck new_state in
      if still_stuck && refresh_count < 10 then
        refresh_until_playable new_state (refresh_count + 1)
      else
        (new_state, refresh_count)
    in
    let (final_state, count) = refresh_until_playable enhanced_state 1 in
    let msg = Printf.sprintf "Both stuck! Refreshed %d time%s." count (if count = 1 then "" else "s") in
    (final_state, msg)
  else
    (enhanced_state, "")
;;

let apply_action (action : Action.t) (model : Model.t) : Model.t =
  let new_model = match action with
  | New_game ->
      let new_enhanced_state = Hw2_speed_logic.Enhanced_game_state.create () in
      let new_model : Model.t = { enhanced_state = new_enhanced_state
      ; selected_card = None
      ; game_message = "New game! You have 5 cards, 15 in draw pile. Play fast!"
      } in
      LocalStorage.clear (); (* Clear saved game when starting new *)
      new_model
  
  | Load_saved_game ->
      (match LocalStorage.load () with
       | Some saved_model -> 
         { saved_model with game_message = "Game restored from local storage!" }
       | None -> 
         { model with game_message = "No saved game found." })
  
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
                 let state_after_stuck_check, stuck_msg = check_and_refresh_if_stuck state_after_draw in
                 
                 (* AI plays continuously via Clock.every - don't play AI moves here! *)
                 if state_after_stuck_check.base_state.game_over then
                   (match state_after_stuck_check.base_state.winner with
                    | Some Hw2_speed_logic.Player.Player1 -> 
                       { enhanced_state = state_after_stuck_check
                       ; selected_card = None
                       ; game_message = "YOU WIN! All cards played! Click 'New Game' to play again."
                       }
                    | Some Hw2_speed_logic.Player.Player2 ->
                       { enhanced_state = state_after_stuck_check
                       ; selected_card = None
                       ; game_message = "AI WINS! AI played all cards first. Click 'New Game' to try again."
                       }
                    | None ->
                       { enhanced_state = state_after_stuck_check
                       ; selected_card = None
                       ; game_message = "Game Over! Click 'New Game' to play again."
                       })
                 else
                   { enhanced_state = state_after_stuck_check
                   ; selected_card = None
                   ; game_message = if String.is_empty stuck_msg then "Good play! Keep going!" else stuck_msg
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
                    let state_after_stuck, _ = check_and_refresh_if_stuck state_with_draw in
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
              }
            | Some Hw2_speed_logic.Player.Player2 ->
              { enhanced_state = final_state
              ; selected_card = None
              ; game_message = "AI WINS! AI was too fast!"
              }
            | None ->
               { enhanced_state = final_state
               ; selected_card = None
               ; game_message = "Game Over!"
               })
         else
           { enhanced_state = final_state
           ; selected_card = model.selected_card
           ; game_message = model.game_message
           }
  in
  (* Auto-save after every action (except Load_saved_game to avoid recursion) *)
  (match action with
   | Load_saved_game -> () (* Don't save when loading *)
   | _ -> LocalStorage.save new_model);
  new_model
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
                   [ Node.strong [ Node.text "SPEED MODE: " ]
                   ; Node.text "Both players play simultaneously! No turns! Play as fast as you can!"
                   ]
              ; Node.div ~attrs:[ Attr.create "class" "info-item" ]
                   [ Node.strong [ Node.text "How to Play: " ]
                   ; Node.text "Click your card → Click center pile. Cards auto-draw after playing!"
                   ]
              ; Node.div ~attrs:[ Attr.create "class" "info-item" ]
                   [ Node.strong [ Node.text "Rules: " ]
                   ; Node.text "Play cards ±1 rank from pile top. Aces play on 2 or King."
                   ]
              ; Node.div ~attrs:[ Attr.create "class" "info-item" ]
                   [ Node.strong [ Node.text "Win: " ]
                   ; Node.text "Empty all 26 cards (5 in hand + 21 in draw pile) before the AI!"
                   ]
              ; Node.div ~attrs:[ Attr.create "class" "info-item" ]
                   [ Node.strong [ Node.text "Game Log:" ] ]
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
      ~default_model:
        (* Try to load from local storage on startup, fallback to initial *)
        (match LocalStorage.load () with
         | Some saved_model -> 
           { saved_model with game_message = "Welcome back! Your game has been restored." }
         | None -> Model.initial)
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
