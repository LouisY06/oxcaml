open! Core
open! Bonsai_web
open Hw2_speed_logic

(* Web interface for Speed card game using OCaml logic *)

module Model = struct
  type t = {
    game_state : Game_state.t;
    selected_card : Card.t option;
    game_log : string list;
  }
  [@@deriving sexp, compare, equal]

  let initial = {
    game_state = Game_state.create ();
    selected_card = None;
    game_log = ["Click 'New Game' to start playing Speed!"];
  }
end

module Action = struct
  type t =
    | New_game
    | Select_card of Card.t
    | Play_card of Card.t * int  (* card and pile index *)
    | AI_move
    | Log_message of string
  [@@deriving sexp, compare]
end

let apply_action (action : Action.t) (model : Model.t) =
  match action with
  | New_game ->
    let new_game_state = Game_state.create () in
    { model with 
      game_state = new_game_state;
      selected_card = None;
      game_log = ["New game started! Two random cards placed in center piles."] @ model.game_log
    }
  
  | Select_card card ->
    { model with selected_card = Some card }
  
  | Play_card (card, pile_index) ->
    let move = Move.Play_card { card; pile = pile_index } in
    (match Game_state.make_move model.game_state move with
    | Ok new_game_state ->
      let log_msg = Printf.sprintf "You played %s on pile %d" (Card.to_string card) (pile_index + 1) in
      { model with 
        game_state = new_game_state;
        selected_card = None;
        game_log = log_msg :: model.game_log
      }
    | Error _ ->
      let log_msg = Printf.sprintf "Cannot play %s on pile %d!" (Card.to_string card) (pile_index + 1) in
      { model with game_log = log_msg :: model.game_log })
  
  | AI_move ->
    (* Simple AI: play first available card *)
    let available_moves = Game_state.get_all_moves model.game_state in
    let play_moves = List.filter available_moves ~f:(function
      | Move.Play_card _ -> true
      | Move.Draw_cards -> false) in
    (match play_moves with
    | Move.Play_card { card; pile } :: _ ->
      (match Game_state.make_move model.game_state (Move.Play_card { card; pile }) with
      | Ok new_game_state ->
        let log_msg = Printf.sprintf "AI played %s on pile %d" (Card.to_string card) (pile + 1) in
        { model with 
          game_state = new_game_state;
          game_log = log_msg :: model.game_log
        }
      | Error _ -> model)
    | [] -> model)
  
  | Log_message msg ->
    { model with game_log = msg :: model.game_log }

let render_card (card : Card.t option) =
  match card with
  | None -> Vdom.Node.text "Empty"
  | Some card -> 
    let card_text = Card.to_string card in
    let color = 
      match card.suit with
      | Card.Hearts | Card.Diamonds -> "red"
      | Card.Clubs | Card.Spades -> "black"
    in
    Vdom.Node.div
      ~attrs:[
        Vdom.Attr.style (Css_gen.color (`Name color));
        Vdom.Attr.class_ "card"
      ]
      [Vdom.Node.text card_text]

let render_hand (hand : Card.t list) (on_card_click : Card.t -> unit) =
  Vdom.Node.div
    ~attrs:[Vdom.Attr.class_ "hand"]
    (List.map hand ~f:(fun card ->
      Vdom.Node.div
        ~attrs:[
          Vdom.Attr.class_ "card";
          Vdom.Attr.on_click (fun _ -> on_card_click card)
        ]
        [Vdom.Node.text (Card.to_string card)]))

let render_game_log (log : string list) =
  Vdom.Node.div
    ~attrs:[Vdom.Attr.class_ "game-log"]
    (List.map log ~f:(fun msg ->
      Vdom.Node.div [Vdom.Node.text msg]))

let component =
  let%sub model, inject = Bonsai.state_machine0
    ~default_model:Model.initial
    ~apply_action
  in
  
  let%sub () = Bonsai.Edge.on_change
    ~equal:[%equal: Model.t]
    model
    ~f:(fun model ->
      (* Auto-trigger AI move after player move *)
      if not model.game_state.game_over then
        inject Action.AI_move)
  in
  
  return (
    let open Vdom.Node in
    div
      ~attrs:[Vdom.Attr.class_ "game-container"]
      [
        div
          ~attrs:[Vdom.Attr.class_ "game-header"]
          [
            h1 [text "Speed Card Game"];
            div
              ~attrs:[Vdom.Attr.class_ "game-controls"]
              [
                button
                  ~attrs:[Vdom.Attr.on_click (fun _ -> inject Action.New_game)]
                  [text "New Game"]
              ]
          ];
        
        div
          ~attrs:[Vdom.Attr.class_ "game-board"]
          [
            (* AI Player Area *)
            div
              ~attrs:[Vdom.Attr.class_ "player-area player2-area"]
              [
                div ~attrs:[Vdom.Attr.class_ "player-label"] [text "AI Player"];
                render_hand model.game_state.player2_hand (fun _ -> ());
                div
                  ~attrs:[Vdom.Attr.class_ "stock-pile"]
                  [
                    div
                      ~attrs:[Vdom.Attr.class_ "stock-label"]
                      [text (Printf.sprintf "Stock: %d cards" (List.length model.game_state.player2_stock))]
                  ]
              ];
            
            (* Center Playing Area *)
            div
              ~attrs:[Vdom.Attr.class_ "center-area"]
              [
                div
                  ~attrs:[Vdom.Attr.class_ "pile-area"]
                  [
                    div
                      ~attrs:[Vdom.Attr.class_ "pile pile1"]
                      [
                        div ~attrs:[Vdom.Attr.class_ "pile-label"] [text "Pile 1"];
                        render_card model.game_state.pile1
                      ];
                    div
                      ~attrs:[Vdom.Attr.class_ "pile pile2"]
                      [
                        div ~attrs:[Vdom.Attr.class_ "pile-label"] [text "Pile 2"];
                        render_card model.game_state.pile2
                      ]
                  ]
              ];
            
            (* Human Player Area *)
            div
              ~attrs:[Vdom.Attr.class_ "player-area player1-area"]
              [
                div ~attrs:[Vdom.Attr.class_ "player-label"] [text "You (Player 1)"];
                render_hand model.game_state.player1_hand (fun card ->
                  match model.selected_card with
                  | None -> inject (Action.Select_card card)
                  | Some selected_card ->
                    if Card.equal card selected_card then
                      (* Try to play on both piles *)
                      (match model.game_state.pile1, model.game_state.pile2 with
                      | Some pile1_card, _ when Card.can_play_on selected_card pile1_card ->
                        inject (Action.Play_card (selected_card, 0))
                      | _, Some pile2_card when Card.can_play_on selected_card pile2_card ->
                        inject (Action.Play_card (selected_card, 1))
                      | _ -> inject (Action.Log_message "Cannot play this card!")));
                div
                  ~attrs:[Vdom.Attr.class_ "stock-pile"]
                  [
                    div
                      ~attrs:[Vdom.Attr.class_ "stock-label"]
                      [text (Printf.sprintf "Stock: %d cards" (List.length model.game_state.player1_stock))]
                  ]
              ]
          ];
        
        div
          ~attrs:[Vdom.Attr.class_ "game-info"]
          [
            div
              ~attrs:[Vdom.Attr.class_ "info-item"]
              [text "Speed Rules: Click cards to play them! Cards must be ±1 rank from pile top. Aces are wild."];
            render_game_log model.game_log
          ]
      ]
  )
