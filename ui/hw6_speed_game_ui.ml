open! Core
open Speed_logic_library
open Hw1
open! Bonsai_web
open! Bonsai.Let_syntax

(* Define 5 triplets (game states) for testing *)
let triplet1_early_game =
  let game_state = Game_state.create () in
  (* Early game: empty piles, both players have full hands *)
  game_state

let triplet2_mid_game =
  let game_state = Game_state.create () in
  (* Mid game: some cards played, reduced stock piles *)
  let game_state = 
    match Game_state.make_move game_state (Move.Play_card { 
      card = { Card.suit = Card.Hearts; rank = Card.Five }; 
      pile = 0 
    }) with
    | Ok state -> state
    | Error _ -> game_state
  in
  let game_state = 
    match Game_state.make_move game_state (Move.Play_card { 
      card = { Card.suit = Card.Spades; rank = Card.Ten }; 
      pile = 1 
    }) with
    | Ok state -> state
    | Error _ -> game_state
  in
  game_state

let triplet3_late_game =
  let game_state = Game_state.create () in
  (* Late game: few cards remaining *)
  let game_state = 
    match Game_state.make_move game_state (Move.Play_card { 
      card = { Card.suit = Card.Clubs; rank = Card.Queen }; 
      pile = 0 
    }) with
    | Ok state -> state
    | Error _ -> game_state
  in
  let game_state = 
    match Game_state.make_move game_state (Move.Play_card { 
      card = { Card.suit = Card.Diamonds; rank = Card.Jack }; 
      pile = 1 
    }) with
    | Ok state -> state
    | Error _ -> game_state
  in
  (* Simulate drawing cards to reduce stock *)
  let game_state = 
    match Game_state.make_move game_state Move.Draw_cards with
    | Ok state -> state
    | Error _ -> game_state
  in
  game_state

let triplet4_almost_won =
  let game_state = Game_state.create () in
  (* Almost won: very few cards left *)
  let game_state = 
    match Game_state.make_move game_state (Move.Play_card { 
      card = { Card.suit = Card.Hearts; rank = Card.King }; 
      pile = 0 
    }) with
    | Ok state -> state
    | Error _ -> game_state
  in
  let game_state = 
    match Game_state.make_move game_state (Move.Play_card { 
      card = { Card.suit = Card.Spades; rank = Card.Ace }; 
      pile = 1 
    }) with
    | Ok state -> state
    | Error _ -> game_state
  in
  (* Draw multiple times to simulate late game *)
  let rec draw_multiple state count =
    if count <= 0 then state
    else
      match Game_state.make_move state Move.Draw_cards with
      | Ok new_state -> draw_multiple new_state (count - 1)
      | Error _ -> state
  in
  draw_multiple game_state 10

let triplet5_game_over =
  let game_state = Game_state.create () in
  (* Game over: one player has won *)
  let game_state = 
    match Game_state.make_move game_state (Move.Play_card { 
      card = { Card.suit = Card.Hearts; rank = Card.Ace }; 
      pile = 0 
    }) with
    | Ok state -> state
    | Error _ -> game_state
  in
  (* Simulate a complete game by playing all cards *)
  let rec play_all_cards state =
    let moves = Game_state.get_all_moves state in
    match moves with
    | [] -> state
    | move :: _ ->
      match Game_state.make_move state move with
      | Ok new_state -> play_all_cards new_state
      | Error _ -> state
  in
  play_all_cards game_state

(* Card rendering functions *)
let card_to_string (card : Card.t) =
  Card.to_string card

let render_card (card : Card.t option) =
  match card with
  | None -> Vdom.Node.text "Empty"
  | Some card -> 
    let card_text = card_to_string card in
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

let render_hand (hand : Card.t list) =
  let card_nodes = List.map hand ~f:(fun card -> render_card (Some card)) in
  Vdom.Node.div
    ~attrs:[Vdom.Attr.class_ "hand"]
    card_nodes

let render_stock (stock : Card.t list) =
  let count = List.length stock in
  Vdom.Node.div
    ~attrs:[Vdom.Attr.class_ "stock-pile"]
    [
      Vdom.Node.div
        ~attrs:[Vdom.Attr.class_ "stock-label"]
        [Vdom.Node.text (Printf.sprintf "Stock: %d cards" count)];
      Vdom.Node.div
        ~attrs:[Vdom.Attr.class_ "card face-down"]
        [Vdom.Node.text "🂠"]
    ]

(* Main game board rendering *)
let speed_game_board ~(game_state : Game_state.t) ~set_game_state =
  let current_player_text = 
    match game_state.current_player with
    | Player.Player1 -> "Player 1"
    | Player.Player2 -> "Player 2"
  in
  
  let game_status_text = 
    if game_state.game_over then
      match game_state.winner with
      | None -> "Game Over"
      | Some Player.Player1 -> "Player 1 Wins!"
      | Some Player.Player2 -> "Player 2 Wins!"
    else
      Printf.sprintf "%s's Turn" current_player_text
  in

  Vdom.Node.div
    ~attrs:[Vdom.Attr.class_ "game-container"]
    [
      (* Game header *)
      Vdom.Node.div
        ~attrs:[Vdom.Attr.class_ "game-header"]
        [
          Vdom.Node.h1 [Vdom.Node.text "Speed Card Game"];
          Vdom.Node.div
            ~attrs:[Vdom.Attr.class_ "game-status"]
            [Vdom.Node.text game_status_text]
        ];
      
      (* Game board *)
      Vdom.Node.div
        ~attrs:[Vdom.Attr.class_ "game-board"]
        [
          (* Player 2 area (top) *)
          Vdom.Node.div
            ~attrs:[Vdom.Attr.class_ "player-area player2-area"]
            [
              Vdom.Node.div
                ~attrs:[Vdom.Attr.class_ "player-label"]
                [Vdom.Node.text "Player 2"];
              render_hand game_state.player2_hand;
              render_stock game_state.player2_stock
            ];
          
          (* Center playing area *)
          Vdom.Node.div
            ~attrs:[Vdom.Attr.class_ "center-area"]
            [
              Vdom.Node.div
                ~attrs:[Vdom.Attr.class_ "pile-area"]
                [
                  Vdom.Node.div
                    ~attrs:[Vdom.Attr.class_ "pile pile1"]
                    [
                      Vdom.Node.div
                        ~attrs:[Vdom.Attr.class_ "pile-label"]
                        [Vdom.Node.text "Pile 1"];
                      render_card game_state.pile1
                    ];
                  Vdom.Node.div
                    ~attrs:[Vdom.Attr.class_ "pile pile2"]
                    [
                      Vdom.Node.div
                        ~attrs:[Vdom.Attr.class_ "pile-label"]
                        [Vdom.Node.text "Pile 2"];
                      render_card game_state.pile2
                    ]
                ]
            ];
          
          (* Player 1 area (bottom) *)
          Vdom.Node.div
            ~attrs:[Vdom.Attr.class_ "player-area player1-area"]
            [
              Vdom.Node.div
                ~attrs:[Vdom.Attr.class_ "player-label"]
                [Vdom.Node.text "Player 1"];
              render_hand game_state.player1_hand;
              render_stock game_state.player1_stock
            ]
        ];
      
      (* Game info *)
      Vdom.Node.div
        ~attrs:[Vdom.Attr.class_ "game-info"]
        [
          Vdom.Node.div
            ~attrs:[Vdom.Attr.class_ "info-item"]
            [Vdom.Node.text "Game Rules: Play cards that are one rank higher or lower than the top card on either pile."];
          Vdom.Node.div
            ~attrs:[Vdom.Attr.class_ "info-item"]
            [Vdom.Node.text (Printf.sprintf "Available moves: %d" (List.length (Game_state.get_all_moves game_state)))]
        ]
    ]

(* App with state management for each triplet *)
let app_triplet1 (local_ graph) =
  let game_state, set_game_state = Bonsai.state triplet1_early_game graph in
  let%arr game_state and set_game_state in
  speed_game_board ~game_state ~set_game_state

let app_triplet2 (local_ graph) =
  let game_state, set_game_state = Bonsai.state triplet2_mid_game graph in
  let%arr game_state and set_game_state in
  speed_game_board ~game_state ~set_game_state

let app_triplet3 (local_ graph) =
  let game_state, set_game_state = Bonsai.state triplet3_late_game graph in
  let%arr game_state and set_game_state in
  speed_game_board ~game_state ~set_game_state

let app_triplet4 (local_ graph) =
  let game_state, set_game_state = Bonsai.state triplet4_almost_won graph in
  let%arr game_state and set_game_state in
  speed_game_board ~game_state ~set_game_state

let app_triplet5 (local_ graph) =
  let game_state, set_game_state = Bonsai.state triplet5_game_over graph in
  let%arr game_state and set_game_state in
  speed_game_board ~game_state ~set_game_state

(* Main app that cycles through all triplets *)
let app (local_ graph) =
  let current_triplet, set_triplet = Bonsai.state 1 graph in
  let%arr current_triplet and set_triplet in
  
  let current_app = 
    match current_triplet with
    | 1 -> app_triplet1 graph
    | 2 -> app_triplet2 graph
    | 3 -> app_triplet3 graph
    | 4 -> app_triplet4 graph
    | 5 -> app_triplet5 graph
    | _ -> app_triplet1 graph
  in
  
  Vdom.Node.div
    [
      (* Triplet selector *)
      Vdom.Node.div
        ~attrs:[Vdom.Attr.class_ "triplet-selector"]
        [
          Vdom.Node.button
            ~attrs:[
              Vdom.Attr.on_click (fun _ -> set_triplet 1);
              Vdom.Attr.class_ (if current_triplet = 1 then "active" else "")
            ]
            [Vdom.Node.text "Triplet 1: Early Game"];
          Vdom.Node.button
            ~attrs:[
              Vdom.Attr.on_click (fun _ -> set_triplet 2);
              Vdom.Attr.class_ (if current_triplet = 2 then "active" else "")
            ]
            [Vdom.Node.text "Triplet 2: Mid Game"];
          Vdom.Node.button
            ~attrs:[
              Vdom.Attr.on_click (fun _ -> set_triplet 3);
              Vdom.Attr.class_ (if current_triplet = 3 then "active" else "")
            ]
            [Vdom.Node.text "Triplet 3: Late Game"];
          Vdom.Node.button
            ~attrs:[
              Vdom.Attr.on_click (fun _ -> set_triplet 4);
              Vdom.Attr.class_ (if current_triplet = 4 then "active" else "")
            ]
            [Vdom.Node.text "Triplet 4: Almost Won"];
          Vdom.Node.button
            ~attrs:[
              Vdom.Attr.on_click (fun _ -> set_triplet 5);
              Vdom.Attr.class_ (if current_triplet = 5 then "active" else "")
            ]
            [Vdom.Node.text "Triplet 5: Game Over"]
        ];
      current_app
    ]

let () = Bonsai_web.Start.start app
