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
      }
   [@@deriving sexp, compare, equal]

   let initial =
      { enhanced_state = Hw2_speed_logic.Enhanced_game_state.create ()
      ; selected_card = None
      ; ai_thinking = false
      }
   ;;
end

module Action = struct
   type t =
      | New_game
      | Select_card of Hw2_speed_logic.Card.t
      | Play_card of Hw2_speed_logic.Card.t * int (* card and pile index *)
      | AI_move
      | Set_ai_thinking of bool
   [@@deriving sexp, compare]
end

let apply_action (action : Action.t) (model : Model.t) =
   match action with
   | New_game ->
      let new_enhanced_state = Hw2_speed_logic.Enhanced_game_state.create () in
   ({ enhanced_state = new_enhanced_state
   ; selected_card = None
   ; ai_thinking = false
   } : Model.t)
   | Select_card card -> { model with selected_card = Some card }
   | Play_card (card, pile_index) ->
      let player_id = "Player1" in
      let move = Hw2_speed_logic.Move.Play_card { card; pile = pile_index } in
      (match
          Hw2_speed_logic.Enhanced_game_state.make_move model.enhanced_state move player_id
       with
       | Ok new_enhanced_state ->
          { model with enhanced_state = new_enhanced_state; selected_card = None }
       | Error _msg -> model)
   | AI_move ->
      (match Hw2_speed_logic.Enhanced_game_state.ai_choose_move model.enhanced_state with
       | Some ai_move ->
          (match
               Hw2_speed_logic.Enhanced_game_state.make_move
                  model.enhanced_state
                  ai_move
                  "Player2"
            with
            | Ok new_enhanced_state ->
               { model with enhanced_state = new_enhanced_state; ai_thinking = false }
            | Error _msg -> { model with ai_thinking = false })
       | None -> { model with ai_thinking = false })
   | Set_ai_thinking b -> { model with ai_thinking = b }
;;

let view (model : Model.t) (inject : Action.t -> unit Effect.t) =
   let open Hw2_speed_logic in
   let open Vdom in
   let open Attr in
   (* Player hand *)
   let player_hand_html =
      Node.div
         ~attrs:[ class_ "hand"; id "player1Hand" ]
         (List.map model.enhanced_state.base_state.player1_hand ~f:(fun card ->
             let card_text = Card.to_string card in
             let is_selected = Option.equal Card.equal model.selected_card (Some card) in
             let classes = if is_selected then "card selected" else "card" in
             Node.div
                ~attrs:[ class_ classes; on_click (fun _ -> inject (Action.Select_card card)) ]
                [ Node.text card_text ]))
   in
   (* AI hand (face down) *)
   let ai_hand_html =
      Node.div
         ~attrs:[ class_ "hand"; id "aiHand" ]
         (List.map model.enhanced_state.base_state.player2_hand ~f:(fun _ ->
             Node.div ~attrs:[ class_ "card face-down" ] [ Node.text "🂠" ]))
   in
   (* Center piles *)
   let pile_html pile_id pile_card_opt =
      Node.div
         ~attrs:
            [ class_ "pile"
            ; id pile_id
            ; on_click (fun _ ->
                  match model.selected_card with
                  | Some card ->
                     inject
                        (Action.Play_card (card, if String.equal pile_id "pile1" then 0 else 1))
                  | None -> Effect.Ignore)
            ]
         [ Node.div
               ~attrs:[ class_ "pile-label" ]
               [ Node.text (Printf.sprintf "Center Pile %s" (String.sub pile_id ~pos:4 ~len:1))
               ]
         ; (match pile_card_opt with
             | Some card ->
                let card_text = Card.to_string card in
                Node.div ~attrs:[ class_ "card" ] [ Node.text card_text ]
             | None -> Node.div ~attrs:[ class_ "card empty-pile" ] [ Node.text "Empty" ])
         ]
   in
   let pile1_html = pile_html "pile1" model.enhanced_state.base_state.pile1 in
   let pile2_html = pile_html "pile2" model.enhanced_state.base_state.pile2 in
   (* Game log *)
   let game_log_html =
      Node.div
         ~attrs:[ class_ "game-log"; id "gameLog" ]
         (List.map model.enhanced_state.game_log ~f:(fun msg ->
             Node.div ~attrs:[ class_ "log-entry" ] [ Node.text msg ]))
   in
   (* Game status *)
   let game_status_text =
      if model.enhanced_state.base_state.game_over
      then (
         match model.enhanced_state.base_state.winner with
         | Some Player.Player1 -> "🎉 YOU WIN! 🎉"
         | Some Player.Player2 -> "😞 AI WINS! 😞"
         | None -> "Game Over (No Winner)")
      else if model.ai_thinking
      then "AI is thinking..."
      else "Game in progress - Click cards to play!"
   in
   Node.div
      ~attrs:[ class_ "game-container" ]
      [ Node.div
            ~attrs:[ class_ "game-header" ]
            [ Node.h1 [ Node.text "HW6: Speed Card Game UI - Bonsai" ]
            ; Node.div
                  ~attrs:[ class_ "game-status"; id "gameStatus" ]
                  [ Node.text game_status_text ]
            ; Node.div
                  ~attrs:[ class_ "game-controls" ]
                  [ Node.button
                        ~attrs:[ on_click (fun _ -> inject Action.New_game) ]
                        [ Node.text "New Game" ]
                  ; Node.button
                        ~attrs:[ on_click (fun _ -> inject Action.AI_move) ]
                        [ Node.text "AI Move" ]
                  ]
            ]
      ; Node.div
            ~attrs:[ class_ "game-board" ]
            [ Node.div
                  ~attrs:[ class_ "player-area player2-area" ]
                  [ Node.div ~attrs:[ class_ "player-label" ] [ Node.text "AI Player" ]
                  ; ai_hand_html
                  ; Node.div
                        ~attrs:[ class_ "stock-pile" ]
                        [ Node.div
                              ~attrs:[ class_ "stock-label" ]
                              [ Node.text
                                    (Printf.sprintf
                                        "Draw Pile: %d cards"
                                        (List.length model.enhanced_state.base_state.player2_stock))
                              ]
                        ; Node.div ~attrs:[ class_ "card face-down stock" ] [ Node.text "🂠" ]
                        ]
                  ]
            ; Node.div
                  ~attrs:[ class_ "center-area" ]
                  [ Node.div ~attrs:[ class_ "pile-area" ] [ pile1_html; pile2_html ] ]
            ; Node.div
                  ~attrs:[ class_ "player-area player1-area" ]
                  [ Node.div ~attrs:[ class_ "player-label" ] [ Node.text "You (Player 1)" ]
                  ; player_hand_html
                  ; Node.div
                        ~attrs:[ class_ "stock-pile" ]
                        [ Node.div
                              ~attrs:[ class_ "stock-label" ]
                              [ Node.text
                                    (Printf.sprintf
                                        "Draw Pile: %d cards"
                                        (List.length model.enhanced_state.base_state.player1_stock))
                              ]
                        ; Node.div ~attrs:[ class_ "card face-down stock" ] [ Node.text "🂠" ]
                        ]
                  ]
            ]
      ; Node.div
            ~attrs:[ class_ "game-info" ]
            [ Node.div
                  ~attrs:[ class_ "info-item" ]
                  [ Node.strong [ Node.text "Controls:" ]
                  ; Node.text
                        " Click a card to select it (green border), then click a pile to play it!"
                  ]
            ; Node.div
                  ~attrs:[ class_ "info-item" ]
                  [ Node.strong [ Node.text "Speed Rules:" ]
                  ; Node.text " Cards must be ±1 rank from pile top. Aces are wild (King or 2)."
                  ]
            ; Node.div
                  ~attrs:[ class_ "info-item" ]
                  [ Node.strong [ Node.text "HW2 Features:" ]
                  ; Node.text " Simultaneous play, enhanced logging, stuck detection."
                  ]
            ; Node.div
                  ~attrs:[ class_ "info-item" ]
                  [ Node.strong [ Node.text "Bonsai Mapping:" ]
                  ; Node.text " OCaml game logic → HTML + CSS components."
                  ]
            ; Node.div ~attrs:[ class_ "info-item" ] [ Node.strong [ Node.text "Game Log:" ] ]
            ; game_log_html
            ]
      ]
;;

let app =
   let%sub model, inject =
      Bonsai.state
         ~sexp_of_model:Model.sexp_of_t
         ~equal:Model.equal
         Model.initial
   in
   let%map model and inject in
   let inject_action action = inject (apply_action action model) in
   view model inject_action
;;

