open! Core
open! Base
open Speed_logic_library
open! Bonsai
open! Bonsai.Let_syntax
open! Bonsai_web
open Js_of_ocaml

(* Use the Deferred module from Firebase_bindings *)
module Deferred = Firebase_bindings.Deferred

(* HW6: Speed Card Game UI using Bonsai *)
(* Simultaneous play - both players can play at any time! *)
(* Offline support with Service Worker and Local Storage *)
(* Online multiplayer support with Firebase Auth and Firestore *)

module Firebase_bindings = Firebase_bindings

(* WebSocket server URL - change to production URL when deployed *)
let websocket_url = "ws://localhost:8080"

module Model = struct
   type screen =
     | LoginScreen
     | ProfileScreen
     | ModeSelectionScreen
     | GameScreen
   [@@deriving sexp, compare, equal]
   
   type player_stats = {
     wins : int
   ; losses : int
   ; games_played : int
   ; win_rate : float (* wins / games_played, or 0.0 if games_played = 0 *)
   }
   [@@deriving sexp, compare, equal]

   type player_number =
     | Player1
     | Player2
   [@@deriving sexp, compare, equal]

   type game_mode =
     | SinglePlayer
     | OnlineMultiplayer of { match_id : string; player_id : string; opponent_id : string; player_number : player_number }
   [@@deriving sexp, compare, equal]
   
   type auth_state =
     | NotAuthenticated
     | Authenticated of { uid : string; email : string option; display_name : string option }
   [@@deriving sexp, compare, equal]
   
   type t =
      { screen : screen
      ; enhanced_state : Hw2_speed_logic.Enhanced_game_state.t
      ; selected_card : Hw2_speed_logic.Card.t option
      ; game_message : string
      ; auth_state : auth_state
      ; game_mode : game_mode
      ; login_email : string
      ; login_password : string
      ; matchmaking_status : string (* "idle" | "searching" | "matched" | "error" *)
      ; firestore_unsubscribe : (Js.Unsafe.any option [@sexp.opaque] [@compare.ignore] [@equal.ignore]) (* For cleaning up listeners *)
      ; game_started : bool (* Whether the game has been started *)
      ; player_stats : player_stats option (* Player statistics, loaded from Firestore *)
      ; lobby_code : string (* Lobby code for joining games *)
      ; created_lobby_code : string option (* Lobby code of the lobby we created (for display) *)
      ; websocket : (Websocket_bindings.websocket option [@sexp.opaque] [@compare.ignore] [@equal.ignore]) (* WebSocket connection *)
      ; ws_connected : bool (* Whether WebSocket is connected *)
      }
  [@@deriving sexp, compare, equal]

   let initial_stats = {
     wins = 0
   ; losses = 0
   ; games_played = 0
   ; win_rate = 0.0
   }

   let initial =
     let () = Stdio.printf "*** Model.initial called - creating LoginScreen model ***\n%!" in
     { screen = LoginScreen
     ; enhanced_state = Hw2_speed_logic.Enhanced_game_state.create ()
      ; selected_card = None
     ; game_message = ""
      ; auth_state = NotAuthenticated
      ; game_mode = SinglePlayer
      ; login_email = ""
      ; login_password = ""
      ; matchmaking_status = "idle"
      ; firestore_unsubscribe = None
      ; game_started = false
      ; player_stats = None
      ; lobby_code = ""
      ; created_lobby_code = None
      ; websocket = None
      ; ws_connected = false
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
    | Update_login_email of string
    | Update_login_password of string
    | Update_login_error of string
    | Sign_in
    | Sign_up
    | Sign_in_with_google
    | Sign_out
    | Auth_state_changed of Firebase_bindings.Auth.auth_state
    | Start_game (* Start the game - makes it active *)
    | Start_matchmaking
    | Cancel_matchmaking
    | Match_found of { match_id : string; player_id : string; opponent_id : string }
    | Game_state_synced of Hw2_speed_logic.Enhanced_game_state.t (* From Firestore *)
    | Select_single_player (* Choose to play against AI *)
    | Select_multiplayer (* Choose to play online *)
    | Create_lobby (* Create a new lobby with a code *)
    | Join_lobby (* Join a lobby by entering a code *)
    | Update_lobby_code of string (* Update the lobby code input field *)
    | Lobby_created of string (* Lobby was created with this code - transition to game room *)
    | Lobby_joined of string (* Lobby was joined - transition to game room *)
    | Go_to_mode_selection (* Go back to mode selection *)
    | Go_to_profile (* Go to profile screen *)
    | Load_player_stats (* Load player stats from Firestore *)
    | Player_stats_loaded of Model.player_stats (* Stats loaded from Firestore *)
    | Ws_connect (* Connect to WebSocket server *)
    | Ws_connected (* WebSocket connection established *)
    | Ws_disconnected (* WebSocket connection lost *)
    | Ws_message of (Js.Unsafe.any [@sexp.opaque] [@compare.ignore]) (* WebSocket message received *)
    | Ws_error of string (* WebSocket error *)
  [@@deriving sexp, compare]
end

(* ============================================================
   LOCAL STORAGE MODULE - Persistent Game State Storage
   ============================================================
   
   Local Storage is part of the Web Storage API that allows
   web applications to store data in the browser that persists
   across page reloads and browser sessions.
   
   Key features:
   - Data persists until explicitly cleared or user clears browser data
   - Storage limit: ~5-10MB per domain (varies by browser)
   - Synchronous API (blocks until complete)
   - Only stores strings (we serialize OCaml data to S-expressions)
   - Domain-specific (data only accessible from same origin)
   
   We use Local Storage to:
   1. Save game state after every action (auto-save)
   2. Restore game state on page refresh
   3. Enable offline play with saved progress
   
   The game state is serialized using S-expressions (sexp) which
   is a text-based format that can be converted to/from OCaml types.
*)
module LocalStorage = struct
  (* Storage key - the name used to store/retrieve game state *)
  let storage_key = "speed_game_state"
  
  (* ============================================================
     SAVE - Store game state in Local Storage
     ============================================================
     Serializes the current game model to a string and stores it
     in the browser's localStorage. This happens automatically
     after every game action (card play, AI move, etc.)
  *)
  let save (model : Model.t) : unit =
    try
      (* Step 1: Convert OCaml model to S-expression *)
      (* S-expressions are a text format that can represent OCaml data *)
      let sexp = Model.sexp_of_t model in
      
      (* Step 2: Convert S-expression to string *)
      (* This creates a JSON-like text representation of the game state *)
      let json_str = Sexp.to_string sexp in
      
      (* Step 3: Access browser's localStorage API *)
      (* localStorage may not be available (private browsing, disabled, etc.) *)
      match Js.Optdef.to_option Dom_html.window##.localStorage with
      | None -> () (* localStorage not available, silently fail *)
      | Some storage ->
        (* Step 4: Store the serialized game state *)
        (* localStorage.setItem(key, value) stores a string value *)
        let key = Js.string storage_key in
        let value = Js.string json_str in
        storage##setItem key value;
        
        (* Step 5: Show visual feedback to user *)
        (* Call JavaScript function to display "Game saved!" indicator *)
        (try
           let show_indicator = Js.Unsafe.global##.showSaveIndicator in
           if Js.Opt.test show_indicator then
             (Js.Unsafe.fun_call show_indicator [||])
         with _ -> ());
        
        (* Log success for debugging *)
        let () = Stdio.printf "Game saved to local storage\n%!" in
        ()
    with
    | _ -> 
      (* If anything fails, log error but don't crash *)
      let () = Stdio.printf "Failed to save game to local storage\n%!" in
      ()
  
  (* ============================================================
     LOAD - Retrieve game state from Local Storage
     ============================================================
     Reads the saved game state from localStorage and deserializes
     it back into an OCaml Model.t. Returns None if no saved game
     exists or if loading fails.
  *)
  let load () : Model.t option =
    (* Don't auto-load saved games - user must explicitly load via Load_saved_game action *)
    (* This ensures we always start on the login screen *)
      None
  
  (* ============================================================
     CLEAR - Remove saved game state from Local Storage
     ============================================================
     Deletes the saved game state. Called when starting a new game
     to ensure old saved state doesn't interfere.
  *)
  let clear () : unit =
    try
      match Js.Optdef.to_option Dom_html.window##.localStorage with
      | None -> () (* localStorage not available *)
      | Some storage ->
        (* localStorage.removeItem(key) deletes the stored value *)
        let key = Js.string storage_key in
        storage##removeItem key;
        let () = Stdio.printf "Local storage cleared\n%!" in
        ()
    with
    | _ -> () (* Silently fail if clear doesn't work *)
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

(* Helper functions for Firestore operations - defined before apply_action *)
let load_player_stats (uid : string) (inject : Action.t -> unit Effect.t) : unit Deferred.t =
  let open Deferred.Let_syntax in
  let%bind result = Firebase_bindings.Firestore.get_doc "players" uid in
  match result with
  | Ok (Some data) ->
    (* Parse stats from Firestore document *)
    let wins = try Int.of_float (Js.float_of_number (Js.Unsafe.get data (Js.string "wins"))) with _ -> 0 in
    let losses = try Int.of_float (Js.float_of_number (Js.Unsafe.get data (Js.string "losses"))) with _ -> 0 in
    let games_played = try Int.of_float (Js.float_of_number (Js.Unsafe.get data (Js.string "games_played"))) with _ -> 0 in
    let win_rate = if games_played > 0 then Float.of_int wins /. Float.of_int games_played else 0.0 in
    let stats = { Model.wins; losses; games_played; win_rate } in
    ignore (inject (Action.Player_stats_loaded stats));
    Deferred.return ()
  | Ok None ->
    (* No stats found - create default stats *)
    let stats = Model.initial_stats in
    ignore (inject (Action.Player_stats_loaded stats));
    (* Also save default stats to Firestore *)
    let data = [
      ("wins", Firebase_bindings.Firestore.int_to_js 0)
    ; ("losses", Firebase_bindings.Firestore.int_to_js 0)
    ; ("games_played", Firebase_bindings.Firestore.int_to_js 0)
    ; ("win_rate", Firebase_bindings.Firestore.int_to_js 0)
    ] in
    let%bind _ = Firebase_bindings.Firestore.set_doc "players" uid data in
    Deferred.return ()
  | Error err ->
    let () = Stdio.printf "Error loading player stats: %s\n%!" err in
    (* Return default stats on error *)
    let stats = Model.initial_stats in
    ignore (inject (Action.Player_stats_loaded stats));
    Deferred.return ()

let save_player_stats (uid : string) (stats : Model.player_stats) : unit Deferred.t =
  let data = [
    ("wins", Firebase_bindings.Firestore.int_to_js stats.wins)
  ; ("losses", Firebase_bindings.Firestore.int_to_js stats.losses)
  ; ("games_played", Firebase_bindings.Firestore.int_to_js stats.games_played)
  ; ("win_rate", Firebase_bindings.Firestore.int_to_js (Int.of_float (stats.win_rate *. 100.0))) (* Store as percentage * 100 *)
  ] in
  Firebase_bindings.Firestore.set_doc "players" uid data

let sync_game_state_to_firestore (match_id : string) (player_id : string) (state : Hw2_speed_logic.Enhanced_game_state.t) : unit Deferred.t =
  (* Serialize game state to S-expression string *)
  let sexp = Hw2_speed_logic.Enhanced_game_state.sexp_of_t state in
  let state_str = Sexp.to_string sexp in
  let data = [
    ("gameState", Firebase_bindings.Firestore.string_to_js state_str)
  ; ("lastUpdatedBy", Firebase_bindings.Firestore.string_to_js player_id)
  ; ("timestamp", Firebase_bindings.Firestore.int_to_js (Int.of_float (Js.Unsafe.global##.Date##now () /. 1000.0)))
  ] in
  Firebase_bindings.Firestore.set_doc "matches" match_id data

(* Generate a random 6-character lobby code *)
let generate_lobby_code () : string =
  let chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" in
  let len = String.length chars in
  let code = String.init 6 ~f:(fun _ ->
    String.get chars (Random.int len)
  ) in
  code

(* Convert a string to a deterministic seed for RNG *)
let string_to_seed (s : string) : int =
  String.fold s ~init:0 ~f:(fun acc c -> (acc * 31 + Char.to_int c) land 0x3FFFFFFF)

(* WebSocket helper functions *)
let init_websocket (inject : Action.t -> unit Effect.t) : Websocket_bindings.websocket =
  let () = Stdio.printf "*** Initializing WebSocket connection to %s ***\n%!" websocket_url in
  let ws = Websocket_bindings.create_websocket websocket_url in

  (* Set up event handlers *)
  Websocket_bindings.on_open ws (fun () ->
    let () = Stdio.printf "*** WebSocket opened! ***\n%!" in
    Ui_effect.Expert.handle (inject Action.Ws_connected)
  );

  Websocket_bindings.on_message ws (fun msg ->
    let () = Stdio.printf "*** WebSocket message received ***\n%!" in
    Ui_effect.Expert.handle (inject (Action.Ws_message msg))
  );

  Websocket_bindings.on_close ws (fun () ->
    let () = Stdio.printf "*** WebSocket closed! ***\n%!" in
    Ui_effect.Expert.handle (inject Action.Ws_disconnected)
  );

  Websocket_bindings.on_error ws (fun err ->
    let () = Stdio.printf "*** WebSocket error: %s ***\n%!" err in
    Ui_effect.Expert.handle (inject (Action.Ws_error err))
  );

  ws
;;

let ws_create_lobby (ws : Websocket_bindings.websocket option) (user_id : string) : unit =
  match ws with
  | None -> Stdio.printf "*** WebSocket not connected, cannot create lobby ***\n%!"
  | Some ws ->
    let () = Stdio.printf "*** Sending create_lobby via WebSocket for user %s ***\n%!" user_id in
    Websocket_bindings.send_json ws [
      ("type", Js.Unsafe.inject (Js.string "create_lobby"))
    ; ("userId", Js.Unsafe.inject (Js.string user_id))
    ]
;;

let ws_join_lobby (ws : Websocket_bindings.websocket option) (user_id : string) (lobby_code : string) : unit =
  match ws with
  | None -> Stdio.printf "*** WebSocket not connected, cannot join lobby ***\n%!"
  | Some ws ->
    let () = Stdio.printf "*** Sending join_lobby via WebSocket for user %s, code %s ***\n%!" user_id lobby_code in
    Websocket_bindings.send_json ws [
      ("type", Js.Unsafe.inject (Js.string "join_lobby"))
    ; ("userId", Js.Unsafe.inject (Js.string user_id))
    ; ("lobbyCode", Js.Unsafe.inject (Js.string lobby_code))
    ]
;;

let ws_send_game_state (ws : Websocket_bindings.websocket option) (lobby_code : string) (player_id : string) (game_state : Hw2_speed_logic.Enhanced_game_state.t) : unit =
  match ws with
  | None -> ()
  | Some ws ->
    let () = Stdio.printf "*** Sending game state via WebSocket ***\n%!" in
    (* Serialize game state to S-expression string *)
    let sexp = Hw2_speed_logic.Enhanced_game_state.sexp_of_t game_state in
    let state_str = Sexp.to_string sexp in
    Websocket_bindings.send_json ws [
      ("type", Js.Unsafe.inject (Js.string "game_state_update"))
    ; ("lobbyCode", Js.Unsafe.inject (Js.string lobby_code))
    ; ("playerId", Js.Unsafe.inject (Js.string player_id))
    ; ("gameState", Js.Unsafe.inject (Js.string state_str))
    ]
;;

let ws_send_player_ready (ws : Websocket_bindings.websocket option) (lobby_code : string) (player_id : string) : unit =
  match ws with
  | None -> Stdio.printf "*** WebSocket not connected, cannot send ready ***\n%!"
  | Some ws ->
    let () = Stdio.printf "*** Sending player_ready via WebSocket ***\n%!" in
    Websocket_bindings.send_json ws [
      ("type", Js.Unsafe.inject (Js.string "player_ready"))
    ; ("lobbyCode", Js.Unsafe.inject (Js.string lobby_code))
    ; ("playerId", Js.Unsafe.inject (Js.string player_id))
    ]
;;

(* Create a lobby with a code *)
let create_lobby (uid : string) (inject : Action.t -> unit Effect.t) : unit Deferred.t =
  let open Deferred.Let_syntax in
  let () = Stdio.printf "*** CREATE_LOBBY called with uid=%s ***\n%!" uid in
  (* Generate a unique lobby code *)
  let lobby_code = generate_lobby_code () in
  let () = Stdio.printf "*** Generated lobby code: %s ***\n%!" lobby_code in
  (* Create lobby document in Firestore *)
  let lobby_data = [
    ("hostId", Firebase_bindings.Firestore.string_to_js uid)
  ; ("status", Firebase_bindings.Firestore.string_to_js "waiting")
  ; ("createdAt", Firebase_bindings.Firestore.int_to_js (Int.of_float (Js.Unsafe.global##.Date##now () /. 1000.0)))
  ; ("player1", Firebase_bindings.Firestore.string_to_js uid)
  ; ("player2", Firebase_bindings.Firestore.string_to_js "")
  ] in
  let () = Stdio.printf "*** Creating lobby document in Firestore: lobbies/%s ***\n%!" lobby_code in
  let%bind _ = Firebase_bindings.Firestore.set_doc "lobbies" lobby_code lobby_data in
  let () = Stdio.printf "*** Lobby created successfully ***\n%!" in
  (* Transition to game room screen with lobby code *)
  let effect = inject (Action.Lobby_created lobby_code) in
  let setTimeout = Js.Unsafe.global##.setTimeout in
  if Js.Optdef.test setTimeout then
    ignore (Js.Unsafe.fun_call setTimeout [|
      Js.Unsafe.inject (Js.wrap_callback (fun _ -> Ui_effect.Expert.handle effect));
      Js.Unsafe.inject (Js.number_of_float 10.0)
    |])
  else
    Ui_effect.Expert.handle effect;
  (* Set up listener for when someone joins *)
  ignore (Firebase_bindings.Firestore.on_snapshot "lobbies" lobby_code (fun data_opt ->
    let () = Stdio.printf "*** Lobby listener fired! ***\n%!" in
    match data_opt with
    | None -> 
      let () = Stdio.printf "*** Lobby listener: data is None (document deleted or permission denied) ***\n%!" in
      ()
    | Some data ->
      (* Check if data is actually valid before accessing properties *)
      (* First check if data is a valid JavaScript object *)
      try
        (* Try to access a property to see if data is valid *)
        let _ = Js.Unsafe.get data (Js.string "status") in
        let status_raw = Js.Unsafe.get data (Js.string "status") in
        if Js.Optdef.test status_raw then
          let status = Js.to_string status_raw in
          let player2_raw = Js.Unsafe.get data (Js.string "player2") in
          let player2 = if Js.Optdef.test player2_raw then Js.to_string player2_raw else "" in
          let () = Stdio.printf "*** Lobby status: %s, player2: %s ***\n%!" status player2 in
          if String.equal status "ready" && not (String.is_empty player2) then
            (* Opponent joined! Create match *)
            let match_id = Printf.sprintf "match_%s_%s" uid player2 in
            let () = Stdio.printf "*** Opponent joined! Creating match: %s ***\n%!" match_id in
            let effect = inject (Action.Match_found { match_id; player_id = uid; opponent_id = player2 }) in
            let setTimeout = Js.Unsafe.global##.setTimeout in
            if Js.Optdef.test setTimeout then
              ignore (Js.Unsafe.fun_call setTimeout [|
                Js.Unsafe.inject (Js.wrap_callback (fun _ ->
                  let () = Stdio.printf "*** Handling Match_found effect from lobby ***\n%!" in
                  Ui_effect.Expert.handle effect;
                  ()));
                Js.Unsafe.inject (Js.number_of_float 10.0)
              |])
            else
              Ui_effect.Expert.handle effect
        else
          let () = Stdio.printf "*** Lobby listener: status field not found or invalid ***\n%!" in
          ()
      with
      | e ->
        let () = Stdio.printf "*** Lobby listener error: %s ***\n%!" (Exn.to_string e) in
        ()
  ));
  Deferred.return ()

(* Join a lobby by code *)
let join_lobby (uid : string) (lobby_code : string) (inject : Action.t -> unit Effect.t) : unit Deferred.t =
  let open Deferred.Let_syntax in
  let () = Stdio.printf "*** JOIN_LOBBY called with uid=%s, code=%s ***\n%!" uid lobby_code in
  (* Get lobby document *)
  let%bind lobby_result = Firebase_bindings.Firestore.get_doc "lobbies" lobby_code in
  match lobby_result with
  | Ok (Some lobby_data) ->
    let () = Stdio.printf "*** Lobby found! Checking if it's available... ***\n%!" in
    let status = Js.to_string (Js.Unsafe.get lobby_data (Js.string "status")) in
    let host_id = Js.to_string (Js.Unsafe.get lobby_data (Js.string "hostId")) in
    let player2_raw = Js.Unsafe.get lobby_data (Js.string "player2") in
    let player2 = if Js.Optdef.test player2_raw then Js.to_string player2_raw else "" in
    let () = Stdio.printf "*** Lobby status: %s, host: %s, player2: %s ***\n%!" status host_id player2 in
    if String.equal status "waiting" && String.is_empty player2 && not (String.equal host_id uid) then
      (* Join the lobby *)
      let () = Stdio.printf "*** Joining lobby... ***\n%!" in
      let%bind _ = Firebase_bindings.Firestore.set_doc ~merge:true "lobbies" lobby_code [
        ("player2", Firebase_bindings.Firestore.string_to_js uid)
      ; ("status", Firebase_bindings.Firestore.string_to_js "ready")
      ] in
      let () = Stdio.printf "*** Joined lobby successfully! Creating match... ***\n%!" in
      (* Create match *)
      let match_id = Printf.sprintf "match_%s_%s" host_id uid in
      let effect = inject (Action.Match_found { match_id; player_id = uid; opponent_id = host_id }) in
      let setTimeout = Js.Unsafe.global##.setTimeout in
      if Js.Optdef.test setTimeout then
        ignore (Js.Unsafe.fun_call setTimeout [|
          Js.Unsafe.inject (Js.wrap_callback (fun _ ->
            let () = Stdio.printf "*** Handling Match_found effect from join lobby ***\n%!" in
            Ui_effect.Expert.handle effect;
            ()));
          Js.Unsafe.inject (Js.number_of_float 10.0)
        |])
      else
        Ui_effect.Expert.handle effect;
      Deferred.return ()
    else if String.equal host_id uid then
      let () = Stdio.printf "*** Cannot join your own lobby! ***\n%!" in
      let effect = inject (Action.Update_login_error "Cannot join your own lobby!") in
      Ui_effect.Expert.handle effect;
      Deferred.return ()
    else
      let () = Stdio.printf "*** Lobby is not available (status: %s, player2: %s) ***\n%!" status player2 in
      let effect = inject (Action.Update_login_error "Lobby is full or not available") in
      Ui_effect.Expert.handle effect;
      Deferred.return ()
  | Ok None ->
    let () = Stdio.printf "*** Lobby not found! ***\n%!" in
    let effect = inject (Action.Update_login_error "Lobby code not found") in
    Ui_effect.Expert.handle effect;
    Deferred.return ()
  | Error err ->
    let () = Stdio.printf "*** Error getting lobby: %s ***\n%!" err in
    let effect = inject (Action.Update_login_error (Printf.sprintf "Error joining lobby: %s" err)) in
    Ui_effect.Expert.handle effect;
    Deferred.return ()

let start_matchmaking (uid : string) (inject : Action.t -> unit Effect.t) : unit Deferred.t =
  let open Deferred.Let_syntax in
  let () = Stdio.printf "*** START_MATCHMAKING CALLED with uid=%s ***\n%!" uid in
  (* Create a matchmaking request in Firestore *)
  let matchmaking_id = Printf.sprintf "mm_%s_%d" uid (Int.of_float (Js.Unsafe.global##.Date##now () /. 1000.0)) in
  let () = Stdio.printf "*** Creating matchmaking document: %s ***\n%!" matchmaking_id in
  let data = [
    ("playerId", Firebase_bindings.Firestore.string_to_js uid)
  ; ("status", Firebase_bindings.Firestore.string_to_js "waiting")
  ; ("createdAt", Firebase_bindings.Firestore.int_to_js (Int.of_float (Js.Unsafe.global##.Date##now () /. 1000.0)))
  ] in
  let () = Stdio.printf "*** Calling Firestore.set_doc for matchmaking collection ***\n%!" in
  let%bind _ = Firebase_bindings.Firestore.set_doc "matchmaking" matchmaking_id data in
  let () = Stdio.printf "*** Matchmaking document created successfully ***\n%!" in
  (* Query for other waiting players *)
  let () = Stdio.printf "*** Querying for waiting players... ***\n%!" in
  let%bind waiting_players_result = 
    Firebase_bindings.Firestore.query_collection 
      "matchmaking" 
      "status" 
      "==" 
      (Firebase_bindings.Firestore.string_to_js "waiting")
  in
  let () = Stdio.printf "*** Query completed, processing results... ***\n%!" in
  match waiting_players_result with
  | Ok docs ->
    let () = Stdio.printf "*** Query returned %d waiting players ***\n%!" (List.length docs) in
    (* Find a player that's not us *)
    let opponent_opt = 
      List.find_map docs ~f:(fun doc ->
        try
          let doc_id = Js.to_string (Js.Unsafe.get doc (Js.string "id")) in
          let doc_data = Js.Unsafe.get doc (Js.string "data") in
          if Js.Optdef.test doc_data then
            let player_id = Js.to_string (Js.Unsafe.get doc_data (Js.string "playerId")) in
            let () = Stdio.printf "*** Found waiting player: %s (doc_id: %s) ***\n%!" player_id doc_id in
            (* Not our own matchmaking request and not already matched *)
            if not (String.equal player_id uid) && not (String.equal doc_id matchmaking_id) then
              let () = Stdio.printf "*** This is a valid opponent! ***\n%!" in
              Some doc
            else
              let () = Stdio.printf "*** Skipping (our own request or same doc) ***\n%!" in
              None
          else
            None
        with e -> 
          let () = Stdio.printf "*** Error processing doc: %s ***\n%!" (Exn.to_string e) in
          None
      )
    in
    (match opponent_opt with
     | Some opponent_doc ->
       let () = Stdio.printf "*** OPPONENT FOUND! Creating match... ***\n%!" in
       (* Found an opponent! Create a match *)
       let opponent_data = Js.Unsafe.get opponent_doc (Js.string "data") in
       let opponent_id = Js.to_string (Js.Unsafe.get opponent_data (Js.string "playerId")) in
       let opponent_matchmaking_id = Js.to_string (Js.Unsafe.get opponent_doc (Js.string "id")) in
       let () = Stdio.printf "*** Opponent ID: %s, Opponent matchmaking ID: %s ***\n%!" opponent_id opponent_matchmaking_id in
       (* Create match document *)
       let match_id = Printf.sprintf "match_%s_%s" uid opponent_id in
       let () = Stdio.printf "*** Match ID: %s ***\n%!" match_id in
       let match_data = [
         ("player1", Firebase_bindings.Firestore.string_to_js uid)
       ; ("player2", Firebase_bindings.Firestore.string_to_js opponent_id)
       ; ("status", Firebase_bindings.Firestore.string_to_js "active")
       ; ("createdAt", Firebase_bindings.Firestore.int_to_js (Int.of_float (Js.Unsafe.global##.Date##now () /. 1000.0)))
       ] in
       let%bind _ = Firebase_bindings.Firestore.set_doc "matches" match_id match_data in
       (* Update both matchmaking documents to "matched" *)
       let%bind _ = Firebase_bindings.Firestore.set_doc "matchmaking" matchmaking_id [
         ("status", Firebase_bindings.Firestore.string_to_js "matched")
       ; ("matchId", Firebase_bindings.Firestore.string_to_js match_id)
       ] in
       let%bind _ = Firebase_bindings.Firestore.set_doc "matchmaking" opponent_matchmaking_id [
         ("status", Firebase_bindings.Firestore.string_to_js "matched")
       ; ("matchId", Firebase_bindings.Firestore.string_to_js match_id)
       ; ("playerId", Firebase_bindings.Firestore.string_to_js opponent_id)
       ] in
       (* Trigger match found action - schedule with setTimeout to ensure Bonsai processes it *)
       let () = Stdio.printf "*** Injecting Match_found action: match_id=%s, opponent_id=%s ***\n%!" match_id opponent_id in
       let effect = inject (Action.Match_found { match_id; player_id = uid; opponent_id }) in
       let setTimeout = Js.Unsafe.global##.setTimeout in
       if Js.Optdef.test setTimeout then
         ignore (Js.Unsafe.fun_call setTimeout [|
           Js.Unsafe.inject (Js.wrap_callback (fun _ ->
             let () = Stdio.printf "*** setTimeout callback - handling Match_found effect ***\n%!" in
             Ui_effect.Expert.handle effect;
             let () = Stdio.printf "*** Match_found effect handled successfully ***\n%!" in
             ()));
           Js.Unsafe.inject (Js.number_of_float 10.0)
         |])
       else
         Ui_effect.Expert.handle effect;
       Deferred.return ()
     | None ->
       let () = Stdio.printf "*** No opponent found yet - setting up listener on matchmaking document: %s ***\n%!" matchmaking_id in
       (* No opponent found yet - set up listener to watch for matches *)
       (* Set up listener on our own matchmaking document *)
       ignore (Firebase_bindings.Firestore.on_snapshot "matchmaking" matchmaking_id (fun data_opt ->
         let () = Stdio.printf "*** Matchmaking listener fired! ***\n%!" in
         match data_opt with
         | None -> 
           let () = Stdio.printf "*** Matchmaking listener: document is None ***\n%!" in
           ()
         | Some data ->
           let status = Js.to_string (Js.Unsafe.get data (Js.string "status")) in
           let () = Stdio.printf "*** Matchmaking listener: status = %s ***\n%!" status in
           if String.equal status "matched" then
             let () = Stdio.printf "*** MATCHED! Getting match document... ***\n%!" in
             let match_id = Js.to_string (Js.Unsafe.get data (Js.string "matchId")) in
             let () = Stdio.printf "*** Match ID from listener: %s ***\n%!" match_id in
             (* Get match document to find opponent *)
             ignore (Deferred.bind ~f:(function
               | Ok (Some match_data) ->
                 let () = Stdio.printf "*** Got match document, extracting opponent... ***\n%!" in
                 let player1 = Js.to_string (Js.Unsafe.get match_data (Js.string "player1")) in
                 let player2 = Js.to_string (Js.Unsafe.get match_data (Js.string "player2")) in
                 let opponent_id = if String.equal player1 uid then player2 else player1 in
                 let () = Stdio.printf "*** Injecting Match_found action from listener: match_id=%s, opponent_id=%s ***\n%!" match_id opponent_id in
                 let effect = inject (Action.Match_found { match_id; player_id = uid; opponent_id }) in
                 (* Schedule with setTimeout to ensure Bonsai processes it *)
                 let setTimeout = Js.Unsafe.global##.setTimeout in
                 if Js.Optdef.test setTimeout then
                   ignore (Js.Unsafe.fun_call setTimeout [|
                     Js.Unsafe.inject (Js.wrap_callback (fun _ ->
                       let () = Stdio.printf "*** setTimeout callback - handling Match_found effect (from listener) ***\n%!" in
                       Ui_effect.Expert.handle effect;
                       let () = Stdio.printf "*** Match_found effect handled successfully (from listener) ***\n%!" in
                       ()));
                     Js.Unsafe.inject (Js.number_of_float 10.0)
                   |])
                 else
                   Ui_effect.Expert.handle effect;
                 Deferred.return ()
               | Ok None ->
                 let () = Stdio.printf "*** Match document not found! ***\n%!" in
                 Deferred.return ()
               | Error e ->
                 let () = Stdio.printf "*** Error getting match document: %s ***\n%!" e in
                 Deferred.return ()) (Firebase_bindings.Firestore.get_doc "matches" match_id))
           else
             let () = Stdio.printf "*** Status is not 'matched', ignoring... ***\n%!" in
             ()
       ));
       let () = Stdio.printf "*** Listener set up, waiting for opponent... ***\n%!" in
       Deferred.return ())
  | Error err ->
    let () = Stdio.printf "*** ERROR: Matchmaking query failed: %s ***\n%!" err in
    let () = Stdio.printf "*** This might be due to Firestore security rules not being set up! ***\n%!" in
    Deferred.return ()

let setup_firestore_listener (match_id : string) (player_id : string) (inject : Action.t -> unit Effect.t) : Js.Unsafe.any option =
  (* Set up real-time listener for game state changes *)
  let () = Stdio.printf "*** Setting up Firestore listener for match: %s, player: %s ***\n%!" match_id player_id in
  match Firebase_bindings.Firestore.on_snapshot "matches" match_id (fun data_opt ->
    let () = Stdio.printf "*** Firestore listener fired for match: %s ***\n%!" match_id in
    match data_opt with
    | None -> 
      let () = Stdio.printf "*** Firestore listener: Document doesn't exist yet ***\n%!" in
      () (* Document doesn't exist *)
    | Some data ->
      let () = Stdio.printf "*** Firestore listener: Document exists, parsing game state ***\n%!" in
      let game_state_str = Js.to_string (Js.Unsafe.get data (Js.string "gameState")) in
      let last_updated = Js.to_string (Js.Unsafe.get data (Js.string "lastUpdatedBy")) in
      let () = Stdio.printf "*** Firestore listener: lastUpdatedBy=%s, player_id=%s ***\n%!" last_updated player_id in
      (* Always update if we have a valid game state and it came from opponent *)
      (* For non-host, we also need to load the initial state created by host *)
      (* The condition: update if (opponent updated OR we haven't loaded initial state yet) AND state is valid *)
      let is_from_opponent = not (String.equal last_updated player_id) in
      let has_valid_state = not (String.is_empty game_state_str) in
      (* Always update if opponent made a change, or if we have a valid state (for initial load) *)
      if has_valid_state && (is_from_opponent || true) then (* Always update on valid state for now *)
        try
          let sexp = Parsexp.Single.parse_string_exn game_state_str in
          let new_state = Hw2_speed_logic.Enhanced_game_state.t_of_sexp sexp in
          (* Trigger action to update game state *)
          ignore (inject (Action.Game_state_synced new_state))
        with _ -> ())
  with
  | Some unsubscribe -> Some unsubscribe
  | None -> None

let apply_action (action : Action.t) (model : Model.t) : Model.t =
  match action with
  | New_game ->
      let new_enhanced_state = Hw2_speed_logic.Enhanced_game_state.create () in
      let new_model = { model with
        enhanced_state = new_enhanced_state
      ; selected_card = None
      ; game_message = "New game! You have 5 cards, 15 in draw pile. Play fast!"
      } in
      LocalStorage.clear (); (* Clear saved game when starting new *)
      (* In multiplayer mode, sync to Firestore (async, fire and forget) *)
      (match model.game_mode with
       | OnlineMultiplayer { match_id; player_id; _ } ->
          ignore (Deferred.bind ~f:(fun () -> Deferred.return ()) (sync_game_state_to_firestore match_id player_id new_enhanced_state));
         new_model
       | SinglePlayer -> new_model)
  
  | Load_saved_game ->
      (match LocalStorage.load () with
       | Some saved_model -> 
         { saved_model with game_message = "Game restored from local storage!" }
       | None -> 
         { model with game_message = "No saved game found." })
  
  | Update_login_email email ->
      (* Clear error message when user starts typing (but keep loading messages) *)
      let cleared_message = 
        if String.equal model.game_message "Signing in..." 
        || String.equal model.game_message "Creating account..." then
          model.game_message
        else
          ""
      in
      { model with login_email = email; game_message = cleared_message }
  
  | Update_login_password password ->
      (* Clear error message when user starts typing (but keep loading messages) *)
      let cleared_message = 
        if String.equal model.game_message "Signing in..." 
        || String.equal model.game_message "Creating account..." then
          model.game_message
        else
          ""
      in
      { model with login_password = password; game_message = cleared_message }
  
  | Sign_in ->
      (* Validate email and password before attempting sign in *)
      let () = Stdio.printf "*** Sign_in action received - email='%s', password length=%d ***\n%!" 
        model.login_email (String.length model.login_password) in
      if String.is_empty model.login_email || String.is_empty model.login_password then
        let () = Stdio.printf "*** Sign_in: Validation failed - empty email or password ***\n%!" in
        { model with game_message = "Please enter both email and password." }
      else
        let () = Stdio.printf "*** Sign_in: Validation passed, setting 'Signing in...' message ***\n%!" in
        (* Sign in will be handled in state machine callback with error handling *)
        (* DON'T clear password here - state machine callback needs it! *)
        { model with game_message = "Signing in..." }
  
  | Sign_up ->
      (* Validate email and password before attempting sign up *)
      let () = Stdio.printf "*** Sign_up action received - email='%s', password length=%d ***\n%!" 
        model.login_email (String.length model.login_password) in
      if String.is_empty model.login_email || String.is_empty model.login_password then
        let () = Stdio.printf "*** Sign_up: Validation failed - empty email or password ***\n%!" in
        { model with game_message = "Please enter both email and password." }
      else
        let () = Stdio.printf "*** Sign_up: Validation passed, setting 'Creating account...' message ***\n%!" in
        (* Sign up will be handled in state machine callback with error handling *)
        (* DON'T clear password here - state machine callback needs it! *)
        { model with game_message = "Creating account..." }
  
  | Sign_in_with_google ->
      (* Google sign in removed - do nothing *)
      model
  
  | Update_login_error error_msg ->
      { model with game_message = error_msg }
  
  | Start_game ->
      (* In multiplayer, send player_ready to server; in single player, just start *)
      (match model.game_mode, model.auth_state, model.created_lobby_code with
       | OnlineMultiplayer _, Model.Authenticated { uid; _ }, Some lobby_code ->
         (* Send player_ready message via WebSocket *)
         ws_send_player_ready model.websocket lobby_code uid;
         { model with game_message = "Waiting for both players to be ready..." }
       | SinglePlayer, _, _ ->
         { model with
           game_started = true
         ; game_message = "Game started! Click on your card, then click on a center pile to play!"
         }
       | _ -> model)
  
  | Sign_out ->
      (* Fire and forget async sign out *)
      ignore (Deferred.bind ~f:(fun () -> Deferred.return ()) (Firebase_bindings.Auth.sign_out ()));
      (* Clean up Firestore listeners *)
      (match model.firestore_unsubscribe with
       | Some unsubscribe ->
         (try
           let unsubscribe_fn = Js.Unsafe.get unsubscribe (Js.string "unsubscribe") in
           ignore (Js.Unsafe.fun_call unsubscribe_fn [||])
         with _ -> ())
       | None -> ());
      { model with
        auth_state = Model.NotAuthenticated
      ; screen = LoginScreen
      ; game_mode = SinglePlayer
      ; matchmaking_status = "idle"
      ; firestore_unsubscribe = None
      ; game_message = "Signed out successfully."
      }
  
  | Auth_state_changed auth_state ->
      let () = Stdio.printf "Auth_state_changed action received, current screen: %s\n%!" 
        (match model.screen with
         | LoginScreen -> "LoginScreen"
         | ProfileScreen -> "ProfileScreen"
         | ModeSelectionScreen -> "ModeSelectionScreen"
         | GameScreen -> "GameScreen")
      in
      let new_auth_state = match auth_state with
        | Firebase_bindings.Auth.SignedOut -> 
          let () = Stdio.printf "User signed out\n%!" in
          Model.NotAuthenticated
        | Firebase_bindings.Auth.SignedIn { uid; email; display_name } ->
          let () = Stdio.printf "User signed in: %s (uid: %s)\n%!" (Option.value email ~default:"no email") uid in
          Model.Authenticated { uid; email; display_name }
      in
      (match new_auth_state, model.screen with
       | Model.NotAuthenticated, _ ->
         let () = Stdio.printf "Setting screen to LoginScreen (user signed out)\n%!" in
         { model with
           auth_state = new_auth_state
         ; screen = LoginScreen
         }
       | Model.Authenticated _, LoginScreen ->
         (* Transition from LoginScreen to ModeSelectionScreen after successful login *)
         let () = Stdio.printf "*** TRANSITIONING FROM LOGINSCREEN TO MODESELECTIONSCREEN ***\n%!" in
         let new_model = { model with
           auth_state = new_auth_state
         ; screen = ModeSelectionScreen
         } in
         let () = Stdio.printf "*** NEW MODEL CREATED: screen=%s, auth_state=%s ***\n%!"
           (match new_model.screen with
            | LoginScreen -> "LoginScreen"
            | ProfileScreen -> "ProfileScreen"
            | ModeSelectionScreen -> "ModeSelectionScreen"
            | GameScreen -> "GameScreen")
           (match new_model.auth_state with
            | NotAuthenticated -> "NotAuthenticated"
            | Authenticated { email; _ } -> Printf.sprintf "Authenticated(%s)" (Option.value email ~default:"no email"))
         in
         new_model
       | Model.Authenticated _, (ProfileScreen | ModeSelectionScreen) ->
         (* Already on profile or mode selection, just update auth state *)
         let () = Stdio.printf "User authenticated, already on %s, keeping current screen\n%!"
           (match model.screen with
            | ProfileScreen -> "ProfileScreen"
            | ModeSelectionScreen -> "ModeSelectionScreen"
            | _ -> "unknown")
         in
         { model with auth_state = new_auth_state }
       | Model.Authenticated _, GameScreen ->
         (* In a game, don't change screen - just update auth state *)
         let () = Stdio.printf "User authenticated during game, keeping GameScreen\n%!" in
         { model with auth_state = new_auth_state })
  
  | Load_player_stats ->
      (* This will be handled in the state machine callback *)
      model
  
  | Player_stats_loaded stats ->
      { model with player_stats = Some stats }

  | Ws_connect ->
      (* WebSocket connection will be initiated in state machine callback *)
      model

  | Ws_connected ->
      let () = Stdio.printf "*** WebSocket connected! ***\n%!" in
      { model with ws_connected = true; game_message = "Connected to server" }

  | Ws_disconnected ->
      let () = Stdio.printf "*** WebSocket disconnected! ***\n%!" in
      { model with ws_connected = false; game_message = "Disconnected from server. Reconnecting..." }

  | Ws_message msg ->
      (* Handle WebSocket messages *)
      let msg_type = Websocket_bindings.get_string_field msg "type" in
      (match msg_type with
       | Some "lobby_created" ->
           let lobby_code = Websocket_bindings.get_string_field msg "lobbyCode" |> Option.value ~default:"" in
           let () = Stdio.printf "*** WS: Lobby created: %s ***\n%!" lobby_code in
           { model with
             screen = GameScreen
           ; created_lobby_code = Some lobby_code
           ; game_message = Printf.sprintf "Waiting for opponent... Lobby Code: %s" lobby_code
           ; game_started = false
           }
       | Some "match_found" ->
           let match_id = Websocket_bindings.get_string_field msg "matchId" |> Option.value ~default:"" in
           let player_id = Websocket_bindings.get_string_field msg "playerId" |> Option.value ~default:"" in
           let opponent_id = Websocket_bindings.get_string_field msg "opponentId" |> Option.value ~default:"" in
           let is_host = Websocket_bindings.get_bool_field msg "isHost" |> Option.value ~default:false in
           let lobby_code = Websocket_bindings.get_string_field msg "lobbyCode" |> Option.value ~default:"" in
           let player_number = if is_host then Model.Player1 else Model.Player2 in
           let () = Stdio.printf "*** WS: Match found! match_id=%s, lobby=%s, isHost=%b, player_number=%s ***\n%!"
             match_id lobby_code is_host (if is_host then "Player1" else "Player2") in
           (* Create game state with deterministic seed *)
           let seed = string_to_seed match_id in
           let () = Stdio.printf "*** Creating game state with seed=%d from match_id ***\n%!" seed in
           let new_enhanced_state = Hw2_speed_logic.Enhanced_game_state.create ~seed () in
           { model with
             screen = GameScreen
           ; game_mode = OnlineMultiplayer { match_id; player_id; opponent_id; player_number }
           ; enhanced_state = new_enhanced_state
           ; game_started = false
           ; created_lobby_code = Some lobby_code
           ; game_message = "Match found! Click 'Start Game' to begin!"
           }
       | Some "game_state_update" ->
           let () = Stdio.printf "*** WS: Game state update received ***\n%!" in
           (* Parse and apply game state from opponent *)
           (try
              let game_state_obj = Js.Unsafe.get msg (Js.string "gameState") in
              let game_state_str = Js.to_string game_state_obj in
              (* Parse the S-expression *)
              let sexp = Parsexp.Single.parse_string_exn game_state_str in
              let new_enhanced_state = Hw2_speed_logic.Enhanced_game_state.t_of_sexp sexp in
              { model with enhanced_state = new_enhanced_state }
            with e ->
              let () = Stdio.printf "*** Error parsing game state: %s ***\n%!" (Exn.to_string e) in
              model)
       | Some "ready_status" ->
           let host_ready = Websocket_bindings.get_bool_field msg "hostReady" |> Option.value ~default:false in
           let joiner_ready = Websocket_bindings.get_bool_field msg "joinerReady" |> Option.value ~default:false in
           let () = Stdio.printf "*** WS: Ready status - host:%b joiner:%b ***\n%!" host_ready joiner_ready in
           let ready_msg =
             if host_ready && joiner_ready then
               "Both players ready! Starting game..."
             else if host_ready then
               "You are ready. Waiting for opponent..."
             else if joiner_ready then
               "Opponent is ready. Click 'Start Game' when ready!"
             else
               "Click 'Start Game' when ready"
           in
           { model with game_message = ready_msg }
       | Some "game_started" ->
           let () = Stdio.printf "*** WS: Game started by server! ***\n%!" in
           { model with
             game_started = true
           ; game_message = "Game started! Play your cards!"
           }
       | Some "error" ->
           let error_msg = Websocket_bindings.get_string_field msg "message" |> Option.value ~default:"Unknown error" in
           let () = Stdio.printf "*** WS Error: %s ***\n%!" error_msg in
           { model with game_message = error_msg }
       | Some "opponent_disconnected" ->
           let () = Stdio.printf "*** WS: Opponent disconnected ***\n%!" in
           { model with game_message = "Opponent disconnected" }
       | _ ->
           let () = Stdio.printf "*** WS: Unknown message type ***\n%!" in
           model)

  | Ws_error err ->
      let () = Stdio.printf "*** WebSocket error: %s ***\n%!" err in
      { model with game_message = Printf.sprintf "Connection error: %s" err }

  | Select_single_player ->
      let new_enhanced_state = Hw2_speed_logic.Enhanced_game_state.create () in
      { model with
        screen = GameScreen
      ; game_mode = SinglePlayer
      ; enhanced_state = new_enhanced_state
      ; selected_card = None
      ; game_started = false
      ; game_message = "Click 'Start Game' to begin playing!"
      }
  
  | Select_multiplayer ->
      (* This action is now deprecated - use Create_lobby or Join_lobby instead *)
      (match model.auth_state with
       | Model.NotAuthenticated ->
         { model with screen = LoginScreen; game_message = "Please sign in first!" }
       | Model.Authenticated _ ->
         (* Matchmaking will be started in the state machine's apply_action callback *)
         { model with
           screen = GameScreen
         ; matchmaking_status = "searching"
         ; game_message = "Searching for opponent..."
         })
  
  | Create_lobby ->
      (* Create lobby action - will be handled in state machine callback *)
      (match model.auth_state with
       | Model.NotAuthenticated ->
         { model with screen = LoginScreen; game_message = "Please sign in first!" }
       | Model.Authenticated _ ->
         { model with game_message = "Creating lobby..." })
  
  | Join_lobby ->
      (* Join lobby action - will be handled in state machine callback *)
      (match model.auth_state with
       | Model.NotAuthenticated ->
         { model with screen = LoginScreen; game_message = "Please sign in first!" }
       | Model.Authenticated _ ->
         if String.is_empty model.lobby_code then
           { model with game_message = "Please enter a lobby code" }
         else
           { model with game_message = "Joining lobby..." })
  
  | Update_lobby_code code ->
      { model with lobby_code = code }
  
  | Lobby_created code ->
      (* Transition to game room screen and show lobby code *)
      { model with
        screen = GameScreen
      ; created_lobby_code = Some code
      ; game_message = Printf.sprintf "Waiting for opponent to join... Lobby Code: %s" code
      ; game_mode = OnlineMultiplayer { match_id = ""; player_id = ""; opponent_id = ""; player_number = Player1 } (* Will be set when match found *)
      ; game_started = false
      }

  | Lobby_joined code ->
      (* Transition to game room screen and show lobby code *)
      { model with
        screen = GameScreen
      ; created_lobby_code = Some code
      ; game_message = Printf.sprintf "Joined lobby! Code: %s - Waiting for game to start..." code
      ; game_mode = OnlineMultiplayer { match_id = ""; player_id = ""; opponent_id = ""; player_number = Player1 } (* Will be set when match found *)
      ; game_started = false
      }
  
  | Go_to_mode_selection ->
      (* Clean up any active game/matchmaking *)
      (match model.firestore_unsubscribe with
       | Some unsubscribe ->
         (try
           let unsubscribe_fn = Js.Unsafe.get unsubscribe (Js.string "unsubscribe") in
           ignore (Js.Unsafe.fun_call unsubscribe_fn [||])
         with _ -> ())
       | None -> ());
      { model with
        screen = ModeSelectionScreen
      ; game_mode = SinglePlayer
      ; matchmaking_status = "idle"
      ; firestore_unsubscribe = None
      }
  
  | Go_to_profile ->
      { model with screen = ProfileScreen }
  
  | Start_matchmaking ->
      (match model.auth_state with
       | Model.NotAuthenticated ->
         { model with screen = LoginScreen; game_message = "Please sign in first to play online!" }
       | Model.Authenticated { uid; _ } ->
         (* Start matchmaking process - this will be handled via Effect *)
         ignore (Deferred.bind ~f:(fun () -> Deferred.return ()) (start_matchmaking uid (fun action -> 
           match action with
           | Action.Match_found _ -> Effect.return () (* Will be handled by action *)
           | _ -> Effect.return ())));
         { model with matchmaking_status = "searching"; game_message = "Searching for opponent..." })
  
  | Cancel_matchmaking ->
      { model with matchmaking_status = "idle"; game_message = "Matchmaking cancelled." }
  
  | Match_found { match_id; player_id; opponent_id } ->
      (* Check if we're the host (player1) by checking if we created the lobby *)
      (* For now, we'll determine host by checking if match_id starts with our player_id *)
      let is_host = String.is_prefix ~prefix:player_id match_id in
      let player_number = if is_host then Model.Player1 else Model.Player2 in
      let () = Stdio.printf "*** Match_found: match_id=%s, player_id=%s, opponent_id=%s, is_host=%b ***\n%!" match_id player_id opponent_id is_host in
      if is_host then
        (* Host creates the initial game state and saves it to Firestore *)
        let new_enhanced_state = Hw2_speed_logic.Enhanced_game_state.create () in
        let () = Stdio.printf "*** Host: Creating initial game state and saving to Firestore ***\n%!" in
        ignore (Deferred.bind ~f:(fun () -> Deferred.return ()) (sync_game_state_to_firestore match_id player_id new_enhanced_state));
        { model with
          screen = GameScreen
        ; game_mode = OnlineMultiplayer { match_id; player_id; opponent_id; player_number }
        ; enhanced_state = new_enhanced_state
        ; selected_card = None
        ; matchmaking_status = "matched"
        ; game_started = false
        ; game_message = Printf.sprintf "Match found! Click 'Start Game' to begin playing against %s" opponent_id
        }
      else
        (* Non-host waits for game state from Firestore - listener will be set up in state machine *)
        let () = Stdio.printf "*** Non-host: Will wait for game state from Firestore ***\n%!" in
        { model with
          screen = GameScreen
        ; game_mode = OnlineMultiplayer { match_id; player_id; opponent_id; player_number }
        ; enhanced_state = model.enhanced_state (* Keep existing state until we get the real one *)
        ; selected_card = None
        ; matchmaking_status = "matched"
        ; game_started = false
        ; game_message = Printf.sprintf "Match found! Waiting for game to start..."
        }
  
  | Game_state_synced new_state ->
      { model with enhanced_state = new_state }
  
  | Select_card card ->
      if not model.game_started then
        { model with game_message = "Please start the game first!" }
      else if model.enhanced_state.base_state.game_over then
         model
      else
        { model with 
            selected_card = Some card
          ; game_message = "Card selected! Click on a center pile to play it."
          }
   
  | Play_on_pile pile_index ->
      if not model.game_started then
        { model with game_message = "Please start the game first!" }
      else if model.enhanced_state.base_state.game_over then
         model
      else
        (match model.selected_card with
         | None ->
            { model with game_message = "Select a card from your hand first!" }
         | Some card ->
            let player_id = match model.game_mode with
              | SinglePlayer -> "Player1"
              | OnlineMultiplayer { player_number = Model.Player1; _ } -> "Player1"
              | OnlineMultiplayer { player_number = Model.Player2; _ } -> "Player2"
            in
            let move = Hw2_speed_logic.Move.Play_card { card; pile = pile_index } in
            match Hw2_speed_logic.Enhanced_game_state.make_move model.enhanced_state move player_id with
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
                 
                 (* In multiplayer mode, sync to Firestore *)
                 let updated_model = { model with
                   enhanced_state = state_after_stuck_check
                 ; selected_card = None
                 ; game_message = if String.is_empty stuck_msg then "Good play! Keep going!" else stuck_msg
                 } in
                 
                 if state_after_stuck_check.base_state.game_over then
                   let win_msg = match state_after_stuck_check.base_state.winner with
                     | Some Hw2_speed_logic.Player.Player1 -> "YOU WIN! All cards played! Click 'New Game' to play again."
                     | Some Hw2_speed_logic.Player.Player2 ->
                       (match model.game_mode with
                        | SinglePlayer -> "AI WINS! AI played all cards first. Click 'New Game' to try again."
                        | OnlineMultiplayer { opponent_id; _ } -> Printf.sprintf "%s WINS! Click 'New Game' to play again." opponent_id)
                     | None -> "Game Over! Click 'New Game' to play again."
                   in
                   let final_model = { updated_model with game_message = win_msg } in
                   (match model.game_mode, model.created_lobby_code with
                    | OnlineMultiplayer { player_id; _ }, Some lobby_code ->
                      ws_send_game_state model.websocket lobby_code player_id state_after_stuck_check;
                      final_model
                    | _ -> final_model)
                 else
                   (match model.game_mode, model.created_lobby_code with
                    | OnlineMultiplayer { player_id; _ }, Some lobby_code ->
                      ws_send_game_state model.websocket lobby_code player_id state_after_stuck_check;
                      updated_model
                    | _ -> updated_model)
            | Error msg ->
               { model with 
                 game_message = "Can't play there: " ^ msg ^ " Try the other pile!"
               })
   
  | AI_move_continuous | Trigger_periodic_update ->
      (* Only run AI if we're on GameScreen, game is started, and in single player mode *)
      (match model.screen, model.game_started, model.game_mode with
       | GameScreen, true, SinglePlayer ->
          (* Run AI logic - only when actually playing *)
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
           
           let updated_model = if final_state.base_state.game_over then
             (let () = Stdio.printf "🏆 GAME OVER! Winner: %s\n%!"
               (match final_state.base_state.winner with
                | Some Hw2_speed_logic.Player.Player1 -> "Player 1"
                | Some Hw2_speed_logic.Player.Player2 -> "Player 2"
                | None -> "None") in
              match final_state.base_state.winner with
              | Some Hw2_speed_logic.Player.Player1 -> 
                { model with
                  enhanced_state = final_state
                ; selected_card = None
                ; game_message = "YOU WIN! All cards played!"
                }
              | Some Hw2_speed_logic.Player.Player2 ->
                { model with
                  enhanced_state = final_state
                ; selected_card = None
                ; game_message = "AI WINS! AI was too fast!"
                }
              | None ->
                 { model with
                   enhanced_state = final_state
                 ; selected_card = None
                 ; game_message = "Game Over!"
                 })
           else
             { model with
               enhanced_state = final_state
             ; selected_card = model.selected_card
             ; game_message = model.game_message
             }
           in
           (* Auto-save after AI move *)
           LocalStorage.save updated_model;
            updated_model
       | _ -> 
          (* Not on game screen, game not started, or multiplayer - don't run AI *)
          model)
  

(* Bonsai components for mapping game logic to HTML + CSS *)
module Components = struct
   open Bonsai_web.Vdom
   open Attr

   (* Helper to render a card *)
   let card_to_html card is_selected is_clickable is_face_down ~inject =
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
            ; (if is_clickable then on_click (fun _ -> inject (Action.Select_card card)) else Attr.empty)
            ; Attr.create "style" ("color: " ^ suit_color ^ "; cursor: " ^ (if is_clickable then "pointer" else "default"))
            ]
         [ Node.text (if is_face_down then "?" else rank_str ^ suit_symbol) ]

   (* Login screen *)
   let login_screen (model : Model.t) (inject : Action.t -> unit Effect.t) =
      let open Vdom in
          Node.div
        ~attrs:[ Attr.create "class" "login-screen"; Attr.create "style" "display: flex; flex-direction: column; align-items: center; justify-content: center; min-height: 100vh; background: #ffffff;" ]
        [ Node.div
            ~attrs:[ Attr.create "class" "login-form"; Attr.create "id" "login-form-id"; Attr.create "style" "padding: 60px 40px; min-width: 400px; max-width: 500px;" ]
            [ Node.h1 ~attrs:[ Attr.create "style" "text-align: center; margin-bottom: 40px; color: #000000; font-size: 36px; font-weight: 400;" ] [ Node.text "Login" ]
            ; Node.div
                ~attrs:[ Attr.create "role" "form"; Attr.create "style" "margin: 0;" ]
                [ Node.div
                    ~attrs:[ Attr.create "style" "margin-bottom: 30px;" ]
                    [ Node.label ~attrs:[ Attr.create "style" "display: block; margin-bottom: 8px; font-weight: 400; color: #666666; font-size: 14px;" ] [ Node.text "Email" ]
                ; Node.input
                    ~attrs:
                      [ Attr.create "type" "email"
                      ; Attr.create "value" model.login_email
                          ; Attr.create "style" "padding: 12px 16px; width: 100%; border: none; border-bottom: 1px solid #e0e0e0; font-size: 16px; box-sizing: border-box; outline: none; background: #f5f5f5; border-radius: 4px;"
                      ; Attr.on_input (fun _ text -> inject (Action.Update_login_email text))
                      ]
                        ()
                ]
            ; Node.div
                    ~attrs:[ Attr.create "style" "margin-bottom: 40px;" ]
                    [ Node.label ~attrs:[ Attr.create "style" "display: block; margin-bottom: 8px; font-weight: 400; color: #666666; font-size: 14px;" ] [ Node.text "Password" ]
                ; Node.input
                    ~attrs:
                      [ Attr.create "type" "password"
                      ; Attr.create "value" model.login_password
                          ; Attr.create "form" "login-form-id"
                          ; Attr.create "style" "padding: 12px 16px; width: 100%; border: none; border-bottom: 1px solid #e0e0e0; font-size: 16px; box-sizing: border-box; outline: none; background: #f5f5f5; border-radius: 4px;"
                      ; Attr.on_input (fun _ text -> inject (Action.Update_login_password text))
                      ]
                        ()
                ]
            ; Node.button
                    ~attrs:
                          [ Attr.create "type" "button"
                          ; on_click (fun _ ->
                              let () = Stdio.printf "*** BUTTON CLICKED: Sign In button was clicked! ***\n%!" in
                              let effect = inject Action.Sign_in in
                              let () = Stdio.printf "*** Effect created from inject Action.Sign_in ***\n%!" in
                              effect)
                          ; Attr.create "id" "sign-in-button"
                          ; Attr.create "style" "width: 100%; padding: 16px; cursor: pointer; background: #000000; color: white; border: none; border-radius: 8px; font-size: 16px; font-weight: 500; margin-bottom: 10px;"
                          ]
                        [ Node.text "Login" ]
            ; Node.button
                    ~attrs:
                          [ Attr.create "type" "button"
                          ; on_click (fun _ ->
                              let () = Stdio.printf "*** BUTTON CLICKED: Sign Up button was clicked! ***\n%!" in
                              let effect = inject Action.Sign_up in
                              let () = Stdio.printf "*** Effect created from inject Action.Sign_up ***\n%!" in
                              effect)
                          ; Attr.create "id" "sign-up-button"
                          ; Attr.create "style" "width: 100%; padding: 16px; cursor: pointer; background: #f5f5f5; color: #000000; border: none; border-radius: 8px; font-size: 16px; font-weight: 500;"
                          ]
                        [ Node.text "Sign Up" ]
                ; (if not (String.is_empty model.game_message) && (String.equal model.game_message "Signing in..." || String.equal model.game_message "Creating account...") then
                    Node.div ~attrs:[ Attr.create "style" "margin-top: 15px; padding: 10px; background: #e3f2fd; border-radius: 5px; text-align: center; color: #1976d2;" ] [ Node.text model.game_message ]
                  else if not (String.is_empty model.game_message) then
                    Node.div ~attrs:[ Attr.create "style" "margin-top: 15px; padding: 10px; background: #ffebee; border-radius: 5px; text-align: center; color: #c62828;" ] [ Node.text model.game_message ]
                  else Node.div [])
                ]
            ]
        ]
   
   (* Mode selection screen *)
   let mode_selection_screen (model : Model.t) (inject : Action.t -> unit Effect.t) =
      let open Vdom in
          Node.div
        ~attrs:[ Attr.create "class" "mode-selection-screen"; Attr.create "style" "display: flex; flex-direction: column; align-items: center; justify-content: center; min-height: 100vh; background: #ffffff;" ]
        [ (* Display lobby code if one was created *)
          (match model.created_lobby_code with
           | Some code ->
             Node.div
               ~attrs:[ Attr.create "style" "position: fixed; top: 30px; left: 50%; transform: translateX(-50%); background: #000000; color: white; padding: 20px 40px; border-radius: 8px; z-index: 1000; text-align: center; min-width: 350px;" ]
               [ Node.div
                   ~attrs:[ Attr.create "style" "font-size: 14px; font-weight: 400; color: #999999; margin-bottom: 8px;" ]
                   [ Node.text "Your Lobby Code" ]
               ; Node.div
                   ~attrs:[ Attr.create "style" "font-size: 36px; font-weight: 500; letter-spacing: 6px; margin: 10px 0; font-family: monospace;" ]
                   [ Node.text code ]
               ; Node.button
                   ~attrs:
                     [ Attr.create "style" "margin-top: 12px; padding: 8px 24px; background: #ffffff; color: #000000; border: none; border-radius: 6px; font-size: 14px; font-weight: 500; cursor: pointer;"
                     ; on_click (fun _ ->
                         (* Copy code to clipboard using JavaScript *)
                         let copy_code_js = Js.Unsafe.global##.navigator##.clipboard in
                         if Js.Optdef.test copy_code_js then
                           let _ = Js.Unsafe.meth_call copy_code_js "writeText" [| Js.Unsafe.inject (Js.string code) |] in
                           inject (Action.Update_login_error "Code copied to clipboard!")
                         else
                           inject (Action.Update_login_error "Clipboard not available"))
                     ]
                   [ Node.text "Copy Code" ]
               ]
           | None -> Node.div [])
        ; Node.div
            ~attrs:[ Attr.create "class" "mode-selection"; Attr.create "style" "padding: 60px 40px; min-width: 400px; max-width: 500px; text-align: center;" ]
            [ Node.h1 ~attrs:[ Attr.create "style" "text-align: center; margin-bottom: 50px; color: #000000; font-size: 36px; font-weight: 400;" ] [ Node.text "Choose Game Mode" ]
            ; Node.button
                ~attrs:
                  [ on_click (fun _ -> inject Action.Select_single_player)
                  ; Attr.create "style" "width: 100%; padding: 16px; cursor: pointer; background: #000000; color: white; border: none; border-radius: 8px; font-size: 16px; font-weight: 500; margin-bottom: 16px;"
                  ]
                [ Node.text "Play Against AI" ]
            ; Node.button
                ~attrs:
                  [ on_click (fun _ -> inject Action.Create_lobby)
                  ; Attr.create "style" "width: 100%; padding: 16px; cursor: pointer; background: #f5f5f5; color: #000000; border: none; border-radius: 8px; font-size: 16px; font-weight: 500; margin-bottom: 32px;"
                  ]
                [ Node.text "Create Lobby" ]
            ; Node.div
                ~attrs:[ Attr.create "style" "margin: 30px 0 20px 0; text-align: center; color: #cccccc; font-size: 14px; font-weight: 400;" ]
                [ Node.text "or join with code" ]
            ; Node.input
                ~attrs:
                  [ Attr.create "type" "text"
                  ; Attr.create "value" model.lobby_code
                  ; Attr.create "placeholder" "XXXXXX"
                  ; Attr.create "maxlength" "6"
                  ; Attr.create "style" "padding: 12px 16px; width: 100%; border: none; font-size: 18px; text-transform: uppercase; letter-spacing: 4px; text-align: center; box-sizing: border-box; background: #f5f5f5; border-radius: 8px; margin-bottom: 12px; outline: none;"
                  ; Attr.on_input (fun _ text -> inject (Action.Update_lobby_code (String.uppercase text)))
                  ]
                  ()
            ; Node.button
                ~attrs:
                  [ on_click (fun _ -> inject Action.Join_lobby)
                  ; Attr.create "style" "width: 100%; padding: 16px; cursor: pointer; background: #f5f5f5; color: #000000; border: none; border-radius: 8px; font-size: 16px; font-weight: 500;"
                  ]
                [ Node.text "Join Lobby" ]
            ; (match model.auth_state with
               | Model.Authenticated { email; _ } ->
                 Node.div
                   ~attrs:[ Attr.create "style" "margin-top: 60px; padding-top: 20px; border-top: 1px solid #e0e0e0; text-align: center;" ]
                   [ Node.div
                       ~attrs:[ Attr.create "style" "font-size: 14px; color: #666666; margin-bottom: 12px;" ]
                       [ Node.text (Option.value email ~default:"User") ]
                   ; Node.button
                       ~attrs:
                         [ on_click (fun _ -> inject Action.Sign_out)
                         ; Attr.create "style" "padding: 8px 24px; cursor: pointer; background: #ffffff; color: #666666; border: 1px solid #e0e0e0; border-radius: 6px; font-size: 14px; font-weight: 400;"
                         ]
                       [ Node.text "Sign Out" ]
                   ]
               | _ -> Node.div [])
            ; (if String.equal model.matchmaking_status "searching" then
                 Node.div
                   ~attrs:[ Attr.create "style" "margin-top: 20px; padding: 15px; background: #f5f5f5; border-radius: 8px; color: #666666; text-align: center;" ]
                   [ Node.text "Searching for opponent... "
                   ; Node.button
                       ~attrs:
                         [ on_click (fun _ -> inject Action.Cancel_matchmaking)
                         ; Attr.create "style" "margin-left: 10px; padding: 8px 16px; cursor: pointer; background: #000000; color: white; border: none; border-radius: 6px; font-size: 14px;"
                         ]
                       [ Node.text "Cancel" ]
                   ]
               else Node.div [])
            ]
        ]
   
   (* Profile screen with stats *)
   let profile_screen (model : Model.t) (inject : Action.t -> unit Effect.t) =
      let open Vdom in
      let stats = Option.value model.player_stats ~default:Model.initial_stats in
      let display_name = match model.auth_state with
        | Model.Authenticated { email; display_name; _ } ->
          Option.value display_name ~default:(Option.value email ~default:"Player")
        | _ -> "Player"
      in
      Node.div
        ~attrs:[ Attr.create "class" "profile-screen"; Attr.create "style" "display: flex; flex-direction: column; align-items: center; justify-content: center; min-height: 100vh; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);" ]
        [ Node.div
            ~attrs:[ Attr.create "class" "profile-card"; Attr.create "style" "padding: 40px; border: 2px solid #ddd; border-radius: 15px; background: white; box-shadow: 0 10px 30px rgba(0,0,0,0.3); min-width: 500px; max-width: 600px;" ]
            [ Node.h1 ~attrs:[ Attr.create "style" "text-align: center; margin-bottom: 30px; color: #333; border-bottom: 2px solid #eee; padding-bottom: 20px;" ] 
                [ Node.text "Player Profile" ]
            ; Node.div
                ~attrs:[ Attr.create "style" "margin-bottom: 30px; text-align: center;" ]
                [ Node.div
                    ~attrs:[ Attr.create "style" "font-size: 24px; font-weight: bold; color: #667eea; margin-bottom: 10px;" ]
                    [ Node.text display_name ]
                ; (match model.auth_state with
                   | Model.Authenticated { email; _ } ->
                     Node.div
                       ~attrs:[ Attr.create "style" "font-size: 14px; color: #666; margin-top: 5px;" ]
                       [ Node.text (Option.value email ~default:"") ]
                   | _ -> Node.div [])
                ]
            ; Node.div
                ~attrs:[ Attr.create "style" "background: #f5f5f5; border-radius: 10px; padding: 20px; margin-bottom: 30px;" ]
                [ Node.h2 ~attrs:[ Attr.create "style" "margin-bottom: 20px; color: #333; font-size: 18px; border-bottom: 1px solid #ddd; padding-bottom: 10px;" ] 
                    [ Node.text "Statistics" ]
                ; Node.div
                    ~attrs:[ Attr.create "style" "display: grid; grid-template-columns: 1fr 1fr; gap: 15px;" ]
                    [ Node.div
                        ~attrs:[ Attr.create "style" "text-align: center; padding: 15px; background: white; border-radius: 8px;" ]
                        [ Node.div ~attrs:[ Attr.create "style" "font-size: 32px; font-weight: bold; color: #4CAF50;" ] 
                            [ Node.text (Int.to_string stats.wins) ]
                        ; Node.div ~attrs:[ Attr.create "style" "font-size: 14px; color: #666; margin-top: 5px;" ] 
                            [ Node.text "Wins" ]
                        ]
                    ; Node.div
                        ~attrs:[ Attr.create "style" "text-align: center; padding: 15px; background: white; border-radius: 8px;" ]
                        [ Node.div ~attrs:[ Attr.create "style" "font-size: 32px; font-weight: bold; color: #f44336;" ] 
                            [ Node.text (Int.to_string stats.losses) ]
                        ; Node.div ~attrs:[ Attr.create "style" "font-size: 14px; color: #666; margin-top: 5px;" ] 
                            [ Node.text "Losses" ]
                        ]
                    ; Node.div
                        ~attrs:[ Attr.create "style" "text-align: center; padding: 15px; background: white; border-radius: 8px;" ]
                        [ Node.div ~attrs:[ Attr.create "style" "font-size: 32px; font-weight: bold; color: #2196F3;" ] 
                            [ Node.text (Int.to_string stats.games_played) ]
                        ; Node.div ~attrs:[ Attr.create "style" "font-size: 14px; color: #666; margin-top: 5px;" ] 
                            [ Node.text "Games Played" ]
                        ]
                    ; Node.div
                        ~attrs:[ Attr.create "style" "text-align: center; padding: 15px; background: white; border-radius: 8px;" ]
                        [ Node.div ~attrs:[ Attr.create "style" "font-size: 32px; font-weight: bold; color: #FF9800;" ] 
                            [ Node.text (Printf.sprintf "%.1f%%" (stats.win_rate *. 100.0)) ]
                        ; Node.div ~attrs:[ Attr.create "style" "font-size: 14px; color: #666; margin-top: 5px;" ] 
                            [ Node.text "Win Rate" ]
                        ]
                    ]
                ]
            ; Node.div
                ~attrs:[ Attr.create "style" "display: flex; gap: 15px; margin-top: 20px;" ]
                [ Node.button
                   ~attrs:
                      [ on_click (fun _ -> inject Action.Select_single_player)
                      ; Attr.create "style" "flex: 1; padding: 15px 30px; cursor: pointer; background: #4CAF50; color: white; border: none; border-radius: 10px; font-size: 16px; font-weight: bold; box-shadow: 0 4px 6px rgba(0,0,0,0.1);"
                      ]
                    [ Node.text "Single Player" ]
                ; Node.button
                    ~attrs:
                      [ on_click (fun _ -> inject Action.Select_multiplayer)
                      ; Attr.create "style" "flex: 1; padding: 15px 30px; cursor: pointer; background: #FF9800; color: white; border: none; border-radius: 10px; font-size: 16px; font-weight: bold; box-shadow: 0 4px 6px rgba(0,0,0,0.1);"
                      ]
                    [ Node.text "Multiplayer" ]
                ]
            ; Node.div
                ~attrs:[ Attr.create "style" "margin-top: 20px; text-align: center;" ]
                [ Node.button
                    ~attrs:
                      [ on_click (fun _ -> inject Action.Sign_out)
                      ; Attr.create "style" "padding: 10px 20px; cursor: pointer; background: #f44336; color: white; border: none; border-radius: 5px; font-size: 14px;"
                      ]
                    [ Node.text "Sign Out" ]
                ]
            ]
        ]

   let view (model : Model.t) (inject : Action.t -> unit Effect.t) =
      let open Hw2_speed_logic in
      let open Vdom in
      
      (* Debug logging *)
      let () = match model.screen with
        | Model.LoginScreen -> Stdio.printf "*** VIEW: RENDERING LOGIN SCREEN (auth_state=%s) ***\n%!"
          (match model.auth_state with
           | NotAuthenticated -> "NotAuthenticated"
           | Authenticated { email; _ } -> Printf.sprintf "Authenticated(%s)" (Option.value email ~default:"no email"))
        | Model.ProfileScreen -> Stdio.printf "*** VIEW: RENDERING PROFILE SCREEN ***\n%!"
        | Model.ModeSelectionScreen -> Stdio.printf "*** VIEW: RENDERING MODE SELECTION SCREEN ***\n%!"
        | Model.GameScreen -> Stdio.printf "*** VIEW: RENDERING GAME SCREEN ***\n%!"
      in

      (* Route to appropriate screen *)
      match model.screen with
      | Model.LoginScreen -> login_screen model inject
      | Model.ProfileScreen -> profile_screen model inject
      | Model.ModeSelectionScreen -> mode_selection_screen model inject
      | Model.GameScreen ->
      (* Game screen *)
      (* Show lobby code if we're waiting in a lobby *)
      let lobby_code_html =
        match model.created_lobby_code with
        | Some code when not model.game_started ->
          Node.div
            ~attrs:[ Attr.create "style" "position: fixed; top: 20px; left: 50%; transform: translateX(-50%); background: #4CAF50; color: white; padding: 20px 40px; border-radius: 10px; box-shadow: 0 4px 6px rgba(0,0,0,0.3); z-index: 1000; text-align: center; min-width: 300px;" ]
            [ Node.h2 ~attrs:[ Attr.create "style" "margin: 0 0 10px 0; font-size: 18px; font-weight: bold;" ] [ Node.text "Lobby Code" ]
            ; Node.div
                ~attrs:[ Attr.create "style" "font-size: 32px; font-weight: bold; letter-spacing: 4px; margin: 10px 0; font-family: monospace;" ]
                [ Node.text code ]
            ; Node.div
                ~attrs:[ Attr.create "style" "font-size: 14px; margin-top: 10px; opacity: 0.9;" ]
                [ Node.text "Share this code with your friend!" ]
            ; Node.button
                ~attrs:
                  [ Attr.create "style" "margin-top: 15px; padding: 10px 20px; background: white; color: #4CAF50; border: none; border-radius: 5px; font-size: 14px; font-weight: bold; cursor: pointer; box-shadow: 0 2px 4px rgba(0,0,0,0.2);"
                  ; on_click (fun _ ->
                      (* Copy code to clipboard using JavaScript *)
                      let copy_code_js = Js.Unsafe.global##.navigator##.clipboard in
                      if Js.Optdef.test copy_code_js then
                        let _ = Js.Unsafe.meth_call copy_code_js "writeText" [| Js.Unsafe.inject (Js.string code) |] in
                        inject (Action.Update_login_error "Code copied to clipboard!")
                      else
                        inject (Action.Update_login_error "Clipboard not available"))
                  ]
                [ Node.text "Copy Code" ]
            ]
        | _ -> Node.div []
      in
      let matchmaking_html =
        match model.game_mode with
        | SinglePlayer -> Node.div []
        | OnlineMultiplayer { opponent_id; _ } ->
          Node.div
            ~attrs:[ Attr.create "class" "matchmaking"; Attr.create "style" "padding: 10px; background: #fff3e0; border-radius: 5px; margin-bottom: 10px;" ]
            [ Node.text (Printf.sprintf "Playing against: %s" opponent_id) ]
      in

      (* Determine which hand to show based on player number *)
      let my_hand, opponent_hand =
        match model.game_mode with
        | SinglePlayer ->
            (model.enhanced_state.base_state.player1_hand, model.enhanced_state.base_state.player2_hand)
        | OnlineMultiplayer { player_number = Model.Player1; _ } ->
            (model.enhanced_state.base_state.player1_hand, model.enhanced_state.base_state.player2_hand)
        | OnlineMultiplayer { player_number = Model.Player2; _ } ->
            (model.enhanced_state.base_state.player2_hand, model.enhanced_state.base_state.player1_hand)
      in

      (* Player hand *)
      let player_hand_html =
         Node.div
            ~attrs:[ Attr.create "class" "hand"; Attr.create "id" "player1Hand" ]
            (List.map my_hand ~f:(fun card ->
                 card_to_html card
                    (Option.equal Card.equal model.selected_card (Some card))
                    model.game_started false ~inject))
      in

      (* AI/Opponent hand (face down) *)
      let ai_hand_html =
         Node.div
            ~attrs:[ Attr.create "class" "hand"; Attr.create "id" "aiHand" ]
            (List.map opponent_hand ~f:(fun card ->
                 card_to_html card false false true ~inject))
      in

      (* Center piles *)
      let pile_html pile_id pile_index pile_card_opt =
         let can_play = model.game_started && Option.is_some model.selected_card && not model.enhanced_state.base_state.game_over in
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
         [ (* User info and back button *)
           (match model.auth_state with
            | Model.Authenticated { email; _ } ->
              Node.div
                ~attrs:[ Attr.create "class" "user-info"; Attr.create "style" "padding: 10px; background: #e8f5e9; border-radius: 5px; margin-bottom: 10px; display: flex; justify-content: space-between; align-items: center;" ]
                [ Node.span
                    ~attrs:[ Attr.create "style" "margin-right: 20px;" ]
                    [ Node.text (Printf.sprintf "Signed in as: %s" (Option.value email ~default:"User")) ]
                ; Node.button
                    ~attrs:
                      [ on_click (fun _ -> inject Action.Go_to_mode_selection)
                      ; Attr.create "style" "padding: 5px 15px; cursor: pointer; background: #2196F3; color: white; border: none; border-radius: 3px; margin-right: 10px;"
                      ]
                    [ Node.text "Back to Menu" ]
                ; Node.button
                    ~attrs:
                      [ on_click (fun _ -> inject Action.Sign_out)
                      ; Attr.create "style" "padding: 5px 15px; cursor: pointer; background: #f44336; color: white; border: none; border-radius: 3px;"
                      ]
                    [ Node.text "Sign Out" ]
                ]
            | _ -> Node.div [])
         ; lobby_code_html
         ; matchmaking_html
         ; Node.div
              ~attrs:[ Attr.create "class" "game-header" ]
              [ Node.h1 [ Node.text "Speed Card Game" ]
              ; Node.div ~attrs:[ Attr.create "class" "game-status"; Attr.create "id" "gameStatus" ]
                   [ Node.text model.game_message ]
              ; Node.div
                   ~attrs:[ Attr.create "class" "game-controls" ]
                   [ (if not model.game_started then
                       Node.button 
                         ~attrs:[ on_click (fun _ -> inject Action.Start_game)
                                ; Attr.create "style" "padding: 15px 30px; border: none; cursor: pointer; border-radius: 5px; font-size: 18px; background: #4CAF50; color: white; font-weight: bold; margin-right: 10px;"
                                ] 
                         [ Node.text "Start Game" ]
                     else
                       Node.button 
                        ~attrs:[ on_click (fun _ -> inject Action.New_game)
                               ; Attr.create "style" "padding: 10px 20px; border: 2px solid black; cursor: pointer; border-radius: 5px; font-size: 16px; background: white;"
                               ] 
                         [ Node.text "New Game" ])
                   ]
              ]
         ; Node.div
              ~attrs:[ Attr.create "class" "game-board" ]
              [ Node.div ~attrs:[ Attr.create "class" "player-area player2-area" ]
                   [ Node.div ~attrs:[ Attr.create "class" "player-label" ] 
                       [ Node.text 
                           (match model.game_mode with
                            | SinglePlayer -> "AI Player"
                            | OnlineMultiplayer { opponent_id; _ } -> Printf.sprintf "Opponent: %s" opponent_id)
                       ]
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
(* FIXED Bonsai App Initialization *)
(* ================================= *)
let app =
  let () = Stdio.printf "*** ======================================== ***\n%!" in
  let () = Stdio.printf "*** INITIALIZING APP - Starting with LoginScreen ***\n%!" in
  let () = Stdio.printf "*** ======================================== ***\n%!" in
  let%sub model, inject =
    Bonsai.state_machine0
      (module Model)
      (module Action)
      ~default_model:
        (* Always start with initial model - login screen first *)
        (* Don't restore saved games automatically - user must sign in first *)
        (let initial = Model.initial in
         let () = Stdio.printf "*** Model.initial created: screen=%s, game_started=%b ***\n%!" 
           (match initial.screen with
            | Model.LoginScreen -> "LoginScreen"
            | Model.ProfileScreen -> "ProfileScreen"
            | Model.ModeSelectionScreen -> "ModeSelectionScreen"
            | Model.GameScreen -> "GameScreen")
           initial.game_started
         in
         let () = Stdio.printf "*** VERIFYING: initial.screen = LoginScreen? %b ***\n%!"
           (match initial.screen with Model.LoginScreen -> true | _ -> false)
         in
         initial)
      ~apply_action:(fun ~inject ~schedule_event:_ _model action ->
        (* Debug: Print the raw action to see what we're getting *)
        let () = Stdio.printf "*** RAW ACTION TYPE: %s ***\n%!" 
          (match action with
           | Action.New_game -> "New_game"
           | Action.Select_card _ -> "Select_card"
           | Action.Play_on_pile _ -> "Play_on_pile"
           | Action.AI_move_continuous -> "AI_move_continuous"
           | Action.Trigger_periodic_update -> "Trigger_periodic_update"
           | Action.Load_saved_game -> "Load_saved_game"
           | Action.Update_login_email _ -> "Update_login_email"
           | Action.Update_login_password _ -> "Update_login_password"
           | Action.Update_login_error _ -> "Update_login_error"
           | Action.Sign_in -> "Sign_in"
           | Action.Sign_up -> 
             let () = Stdio.printf "*** RAW ACTION TYPE: Sign_up detected! ***\n%!" in
             "Sign_up"
           | Action.Sign_in_with_google -> "Sign_in_with_google"
           | Action.Sign_out -> "Sign_out"
           | Action.Auth_state_changed _ -> "Auth_state_changed"
           | Action.Start_game -> "Start_game"
           | Action.Start_matchmaking -> "Start_matchmaking"
           | Action.Cancel_matchmaking -> "Cancel_matchmaking"
           | Action.Match_found _ -> "Match_found"
           | Action.Select_single_player -> "Select_single_player"
           | Action.Select_multiplayer -> "Select_multiplayer"
           | Action.Go_to_profile -> "Go_to_profile"
            | Action.Go_to_mode_selection -> "Go_to_mode_selection"
            | Action.Load_player_stats -> "Load_player_stats"
            | Action.Player_stats_loaded _ -> "Player_stats_loaded"
            | Action.Game_state_synced _ -> "Game_state_synced"
            | Action.Create_lobby -> "Create_lobby"
            | Action.Join_lobby -> "Join_lobby"
            | Action.Update_lobby_code _ -> "Update_lobby_code"
            | Action.Lobby_created _ -> "Lobby_created"
            | Action.Lobby_joined _ -> "Lobby_joined"
            | Action.Ws_connect -> "Ws_connect"
            | Action.Ws_connected -> "Ws_connected"
            | Action.Ws_disconnected -> "Ws_disconnected"
            | Action.Ws_message _ -> "Ws_message"
            | Action.Ws_error _ -> "Ws_error")
        in
        let action_str = match action with
          | Action.Sign_in -> "Sign_in"
          | Action.Sign_up -> "Sign_up"
          | Action.Sign_in_with_google -> "Sign_in_with_google"
          | Action.Auth_state_changed auth_state -> 
            let () = Stdio.printf "*** MATCHED Auth_state_changed! auth_state=%s ***\n%!"
              (match auth_state with
               | Firebase_bindings.Auth.SignedOut -> "SignedOut"
               | Firebase_bindings.Auth.SignedIn { email; _ } -> 
                 Printf.sprintf "SignedIn(%s)" (Option.value email ~default:"no email"))
            in
            (match auth_state with
             | Firebase_bindings.Auth.SignedOut -> "Auth_state_changed(SignedOut)"
             | Firebase_bindings.Auth.SignedIn { email; _ } -> 
               Printf.sprintf "Auth_state_changed(SignedIn: %s)" (Option.value email ~default:"no email"))
          | _ -> "Other"
        in
        let screen_str = match _model.screen with
          | LoginScreen -> "LoginScreen"
          | ProfileScreen -> "ProfileScreen"
          | ModeSelectionScreen -> "ModeSelectionScreen"
          | GameScreen -> "GameScreen"
        in
        let auth_str = match _model.auth_state with
          | NotAuthenticated -> "NotAuthenticated"
          | Authenticated { email; _ } -> Printf.sprintf "Authenticated(%s)" (Option.value email ~default:"no email")
        in
        let () = Stdio.printf "*** STATE MACHINE: apply_action called with action: %s, current screen: %s, current auth_state: %s ***\n%!"
          action_str screen_str auth_str in
        let new_model = apply_action action _model in
        let () = Stdio.printf "*** STATE MACHINE: After apply_action, new_model.screen: %s, auth_state: %s ***\n%!"
          (match new_model.screen with
           | LoginScreen -> "LoginScreen"
           | ProfileScreen -> "ProfileScreen"
           | ModeSelectionScreen -> "ModeSelectionScreen"
           | GameScreen -> "GameScreen")
          (match new_model.auth_state with
           | NotAuthenticated -> "NotAuthenticated"
           | Authenticated { email; _ } -> Printf.sprintf "Authenticated(%s)" (Option.value email ~default:"no email"))
        in
        (* Handle async auth operations and errors *)
        let final_model = (match action with
         | Sign_in ->
           (* Only attempt sign-in if email and password are not empty *)
           let () = Stdio.printf "*** STATE MACHINE: Sign_in handler - email='%s', password length=%d ***\n%!" 
             new_model.login_email (String.length new_model.login_password) in
           if String.is_empty new_model.login_email || String.is_empty new_model.login_password then
             let () = Stdio.printf "*** STATE MACHINE: Sign_in - Email or password is empty, skipping Firebase call ***\n%!" in
             (* Update message to show validation error *)
             { new_model with game_message = "Please enter both email and password." }
           else
             (* Handle sign in errors *)
             let () = Stdio.printf "*** STATE MACHINE: Sign_in - Validation passed, calling Firebase ***\n%!" in
             let () = Stdio.printf "*** CALLING Firebase sign_in_with_email_and_password NOW ***\n%!" in
             let () = Stdio.printf "*** Email: %s, Password: [%d chars] ***\n%!" 
               new_model.login_email (String.length new_model.login_password) in
             let deferred_result = Firebase_bindings.Auth.sign_in_with_email_and_password new_model.login_email new_model.login_password in
             let () = Stdio.printf "*** Deferred created, binding callback... ***\n%!" in
             (* Fire and forget - handle result in callback *)
             let handle_result = function
               | Ok user -> 
                 let () = Stdio.printf "*** DEFERRED CALLBACK FIRED - SIGN IN SUCCESSFUL! ***\n%!" in
                 (* Manually trigger auth state change since callback might not fire immediately *)
                 let user_info = Firebase_bindings.Auth.get_user_info user in
                 let () = Stdio.printf "*** User info retrieved: %s ***\n%!" 
                   (match user_info with
                    | Firebase_bindings.Auth.SignedIn { email; _ } -> 
                      Printf.sprintf "SignedIn(%s)" (Option.value email ~default:"no email")
                    | Firebase_bindings.Auth.SignedOut -> "SignedOut") in
                 let () = Stdio.printf "*** Creating and handling effect ***\n%!" in
                 (* Schedule the effect using JavaScript setTimeout to ensure it runs on the next tick *)
                 (* This is critical because we're in a Deferred callback and need to let Bonsai's event loop process it *)
                 let effect = inject (Action.Auth_state_changed user_info) in
                 let setTimeout = Js.Unsafe.global##.setTimeout in
                 if Js.Optdef.test setTimeout then
                   let callback = Js.wrap_callback (fun _ ->
                     let () = Stdio.printf "*** setTimeout callback - handling Auth_state_changed effect ***\n%!" in
                     Ui_effect.Expert.handle effect;
                     let () = Stdio.printf "*** Effect handled in setTimeout callback ***\n%!" in
                     ()) in
                   ignore (Js.Unsafe.fun_call setTimeout [| 
                     Js.Unsafe.inject callback;
                     Js.Unsafe.inject (Js.number_of_float 10.0) (* 10ms delay to ensure Bonsai is ready *)
                   |])
                 else
                   (* Fallback - handle immediately if setTimeout not available *)
                   let () = Stdio.printf "*** setTimeout not available, handling immediately ***\n%!" in
                   Ui_effect.Expert.handle effect;
                 let () = Stdio.printf "*** Effect scheduled, should transition to ModeSelectionScreen ***\n%!" in
                 ()
               | Error msg -> 
                 let () = Stdio.printf "*** DEFERRED CALLBACK FIRED - SIGN IN FAILED: %s ***\n%!" msg in
                 let effect = inject (Action.Update_login_error msg) in
                 (* Schedule error effect using setTimeout *)
                 let setTimeout = Js.Unsafe.global##.setTimeout in
                 if Js.Optdef.test setTimeout then
                   ignore (Js.Unsafe.fun_call setTimeout [| 
                     Js.Unsafe.inject (Js.wrap_callback (fun _ -> Ui_effect.Expert.handle effect));
                     Js.Unsafe.inject (Js.number_of_float 10.0)
                   |])
                 else
                   Ui_effect.Expert.handle effect;
                 ()
             in
             ignore (Deferred.bind deferred_result ~f:(fun result -> handle_result result; Deferred.return ()));
             let () = Stdio.printf "*** Deferred created and bound ***\n%!" in
             (* Don't clear password on error - user might want to try again *)
             new_model
         | Sign_up ->
           (* Only attempt sign-up if email and password are not empty *)
           let () = Stdio.printf "*** STATE MACHINE: Sign_up handler - email='%s', password length=%d ***\n%!" 
             new_model.login_email (String.length new_model.login_password) in
           if String.is_empty new_model.login_email || String.is_empty new_model.login_password then
             let () = Stdio.printf "*** STATE MACHINE: Sign_up - Email or password is empty, skipping Firebase call ***\n%!" in
             (* Update message to show validation error *)
             { new_model with game_message = "Please enter both email and password." }
           else
             (* Handle sign up errors *)
             let () = Stdio.printf "*** STATE MACHINE: Sign_up - Validation passed, calling Firebase ***\n%!" in
             let () = Stdio.printf "*** CALLING Firebase create_user_with_email_and_password NOW ***\n%!" in
             let () = Stdio.printf "*** Email: %s, Password: [%d chars] ***\n%!" 
               new_model.login_email (String.length new_model.login_password) in
             let deferred_result = Firebase_bindings.Auth.create_user_with_email_and_password new_model.login_email new_model.login_password in
             let () = Stdio.printf "*** Deferred created, binding callback... ***\n%!" in
             (* Fire and forget - handle result in callback *)
             let handle_result = function
               | Ok user -> 
                 let () = Stdio.printf "*** DEFERRED CALLBACK FIRED - SIGN UP SUCCESSFUL! ***\n%!" in
                 (* Manually trigger auth state change since callback might not fire immediately *)
                 let user_info = Firebase_bindings.Auth.get_user_info user in
                 let () = Stdio.printf "*** User info retrieved: %s ***\n%!" 
                   (match user_info with
                    | Firebase_bindings.Auth.SignedIn { email; _ } -> 
                      Printf.sprintf "SignedIn(%s)" (Option.value email ~default:"no email")
                    | Firebase_bindings.Auth.SignedOut -> "SignedOut") in
                 let () = Stdio.printf "*** Creating and handling effect ***\n%!" in
                 (* Schedule the effect using JavaScript setTimeout to ensure it runs on the next tick *)
                 let effect = inject (Action.Auth_state_changed user_info) in
                 let setTimeout = Js.Unsafe.global##.setTimeout in
                 if Js.Optdef.test setTimeout then
                   let callback = Js.wrap_callback (fun _ ->
                     let () = Stdio.printf "*** setTimeout callback - handling Auth_state_changed effect (sign up) ***\n%!" in
                     Ui_effect.Expert.handle effect;
                     let () = Stdio.printf "*** Effect handled in setTimeout callback ***\n%!" in
                     ()) in
                   ignore (Js.Unsafe.fun_call setTimeout [| 
                     Js.Unsafe.inject callback;
                     Js.Unsafe.inject (Js.number_of_float 10.0) (* 10ms delay to ensure Bonsai is ready *)
                   |])
                 else
                   Ui_effect.Expert.handle effect;
                 let () = Stdio.printf "*** Effect scheduled, should transition to ModeSelectionScreen ***\n%!" in
                 ()
               | Error msg -> 
                 let () = Stdio.printf "*** DEFERRED CALLBACK FIRED - SIGN UP FAILED: %s ***\n%!" msg in
                 let effect = inject (Action.Update_login_error msg) in
                 (* Schedule error effect using setTimeout *)
                 let setTimeout = Js.Unsafe.global##.setTimeout in
                 if Js.Optdef.test setTimeout then
                   ignore (Js.Unsafe.fun_call setTimeout [| 
                     Js.Unsafe.inject (Js.wrap_callback (fun _ -> Ui_effect.Expert.handle effect));
                     Js.Unsafe.inject (Js.number_of_float 10.0)
                   |])
                 else
                   Ui_effect.Expert.handle effect;
                 ()
             in
             ignore (Deferred.bind deferred_result ~f:(fun result -> handle_result result; Deferred.return ()));
             let () = Stdio.printf "*** Deferred created and bound ***\n%!" in
             (* Don't clear password on error - user might want to try again or sign in *)
             new_model
         | Sign_in_with_google ->
           (* Google sign in removed - do nothing *)
           new_model
         | Load_player_stats ->
           (* Load player stats from Firestore *)
           (match new_model.auth_state with
            | Model.Authenticated { uid; _ } ->
              ignore (Deferred.bind ~f:(fun () -> Deferred.return ()) (load_player_stats uid inject));
              new_model
            | _ -> new_model)
         | Auth_state_changed auth_state ->
           (* Auth_state_changed is handled in apply_action - the screen should already be updated *)
           let () = Stdio.printf "*** STATE MACHINE: Auth_state_changed action in final_model match - new_model.screen is %s, auth_state=%s ***\n%!"
             (match new_model.screen with
              | LoginScreen -> "LoginScreen"
              | ProfileScreen -> "ProfileScreen"
              | ModeSelectionScreen -> "ModeSelectionScreen"
              | GameScreen -> "GameScreen")
             (match auth_state with
              | Firebase_bindings.Auth.SignedOut -> "SignedOut"
              | Firebase_bindings.Auth.SignedIn { email; _ } ->
                Printf.sprintf "SignedIn(%s)" (Option.value email ~default:"no email"))
           in
           (* Initialize WebSocket when user signs in *)
           (match auth_state with
            | Firebase_bindings.Auth.SignedIn _ ->
              if Option.is_none new_model.websocket then
                let () = Stdio.printf "*** Initializing WebSocket connection ***\n%!" in
                let ws = init_websocket inject in
                { new_model with websocket = Some ws }
              else
                new_model
            | Firebase_bindings.Auth.SignedOut ->
              (* Close WebSocket when user signs out *)
              (match new_model.websocket with
               | Some ws ->
                 Websocket_bindings.close ws;
                 { new_model with websocket = None; ws_connected = false }
               | None -> new_model))
         | _ -> 
        (* Set up Firestore listener when entering multiplayer mode *)
        (match action, new_model.game_mode with
         | Match_found { match_id; player_id; _ }, OnlineMultiplayer { match_id = match_id2; _ } 
           when String.equal match_id match_id2 ->
           let () = Stdio.printf "*** STATE MACHINE: Setting up Firestore listener for match: %s ***\n%!" match_id in
           (* Check if we're the host - host creates state, non-host waits for it *)
           let is_host = String.is_prefix ~prefix:player_id match_id in
           if is_host then
             (* Host: Set up listener for opponent's moves *)
             (match setup_firestore_listener match_id player_id inject with
              | Some unsubscribe ->
                { new_model with firestore_unsubscribe = Some unsubscribe }
              | None -> new_model)
           else
             (* Non-host: Set up listener and try to load initial state *)
             (match setup_firestore_listener match_id player_id inject with
              | Some unsubscribe ->
                (* Try to load initial game state from Firestore *)
                ignore (Deferred.bind (Firebase_bindings.Firestore.get_doc "matches" match_id) ~f:(fun result ->
                  match result with
                  | Ok (Some data) ->
                    let () = Stdio.printf "*** Non-host: Loaded initial game state from Firestore ***\n%!" in
                    (try
                      let game_state_str = Js.to_string (Js.Unsafe.get data (Js.string "gameState")) in
                      let sexp = Parsexp.Single.parse_string_exn game_state_str in
                      let game_state = Hw2_speed_logic.Enhanced_game_state.t_of_sexp sexp in
                      let effect = inject (Action.Game_state_synced game_state) in
                      let setTimeout = Js.Unsafe.global##.setTimeout in
                      if Js.Optdef.test setTimeout then
                        ignore (Js.Unsafe.fun_call setTimeout [|
                          Js.Unsafe.inject (Js.wrap_callback (fun _ -> Ui_effect.Expert.handle effect));
                          Js.Unsafe.inject (Js.number_of_float 10.0)
                        |])
                      else
                        Ui_effect.Expert.handle effect
                    with
                    | e ->
                      let () = Stdio.printf "*** Error parsing game state: %s ***\n%!" (Exn.to_string e) in
                      ());
                    Deferred.return ()
                  | Ok None ->
                    let () = Stdio.printf "*** Non-host: Game state not ready yet, will wait for listener ***\n%!" in
                    Deferred.return ()
                  | Error e ->
                    let () = Stdio.printf "*** Non-host: Error loading game state: %s ***\n%!" e in
                    Deferred.return ()));
                { new_model with firestore_unsubscribe = Some unsubscribe }
              | None -> new_model)
         | Start_matchmaking, _ ->
           (* Re-inject start_matchmaking with proper inject function *)
           (match new_model.auth_state with
            | Model.Authenticated { uid; _ } ->
              ignore (Deferred.bind ~f:(fun () -> Deferred.return ()) (start_matchmaking uid inject));
              new_model
            | _ -> new_model)
         | Select_multiplayer, _ ->
           (* Start matchmaking when multiplayer is selected *)
           let () = Stdio.printf "*** STATE MACHINE: Select_multiplayer action - starting matchmaking ***\n%!" in
           (match new_model.auth_state with
            | Model.Authenticated { uid; _ } ->
              let () = Stdio.printf "*** User is authenticated, uid=%s, calling start_matchmaking ***\n%!" uid in
              ignore (Deferred.bind ~f:(fun () -> 
                let () = Stdio.printf "*** start_matchmaking completed ***\n%!" in
                Deferred.return ()) (start_matchmaking uid inject));
              new_model
            | Model.NotAuthenticated ->
              let () = Stdio.printf "*** User is NOT authenticated, cannot start matchmaking ***\n%!" in
              new_model)
         | Create_lobby, _ ->
           (* Create a lobby when Create_lobby action is triggered *)
           let () = Stdio.printf "*** STATE MACHINE: Create_lobby action - creating lobby via WebSocket ***\n%!" in
           (match new_model.auth_state with
            | Model.Authenticated { uid; _ } ->
              let () = Stdio.printf "*** User is authenticated, uid=%s, sending WebSocket create_lobby ***\n%!" uid in
              ws_create_lobby new_model.websocket uid;
              { new_model with game_message = "Creating lobby..." }
            | Model.NotAuthenticated ->
              let () = Stdio.printf "*** User is NOT authenticated, cannot create lobby ***\n%!" in
              new_model)
         | Lobby_created code, _ ->
           (* Lobby was created - store the code and show it *)
           let () = Stdio.printf "*** STATE MACHINE: Lobby_created action - code=%s ***\n%!" code in
           { new_model with created_lobby_code = Some code; game_message = Printf.sprintf "Lobby created! Code: %s - Waiting for opponent..." code }
         | Join_lobby, _ ->
           (* Join a lobby when Join_lobby action is triggered *)
           let () = Stdio.printf "*** STATE MACHINE: Join_lobby action - joining lobby via WebSocket with code: %s ***\n%!" new_model.lobby_code in
           (match new_model.auth_state with
            | Model.Authenticated { uid; _ } ->
              if String.is_empty new_model.lobby_code then
                { new_model with game_message = "Please enter a lobby code" }
              else
                let () = Stdio.printf "*** User is authenticated, uid=%s, sending WebSocket join_lobby ***\n%!" uid in
                ws_join_lobby new_model.websocket uid new_model.lobby_code;
                { new_model with game_message = "Joining lobby..." }
            | Model.NotAuthenticated ->
              let () = Stdio.printf "*** User is NOT authenticated, cannot join lobby ***\n%!" in
              new_model)
         | _ -> new_model))
        in
        let () = Stdio.printf "*** STATE MACHINE: Returning final_model with screen: %s, auth_state: %s ***\n%!"
          (match final_model.screen with
           | LoginScreen -> "LoginScreen"
           | ProfileScreen -> "ProfileScreen"
           | ModeSelectionScreen -> "ModeSelectionScreen"
           | GameScreen -> "GameScreen")
          (match final_model.auth_state with
           | NotAuthenticated -> "NotAuthenticated"
           | Authenticated { email; _ } -> Printf.sprintf "Authenticated(%s)" (Option.value email ~default:"no email"))
        in
        final_model)
  in
  
  (* Note: Firebase auth callback is now set up using Bonsai.Edge.lifecycle below *)
  (* This ensures it's properly initialized and the inject function remains valid *)
  
(**************************************************)
  (* Periodic AI update every 2000ms (2 seconds) - AI plays 1 card every 2 seconds *)
  let%sub () =
    Bonsai.Clock.every
      ~when_to_start_next_effect:`Every_multiple_of_period_blocking
      ~trigger_on_activate:true
      (Time_ns.Span.of_ms 2000.0)
      (let%map inject = inject in
       inject Action.Trigger_periodic_update)
  in

  (* Set up Firebase auth state listener using Bonsai.Edge.lifecycle *)
  (* This ensures the callback is set up once when the component activates *)
  (* and the inject function is properly captured and remains valid *)
  let%sub () =
    Bonsai.Edge.lifecycle
      ~on_activate:(let%map inject = inject in
        let () = Stdio.printf "Setting up Firebase auth callback in lifecycle\n%!" in
        let callback auth_state =
          let () = Stdio.printf "Firebase auth callback - injecting Auth_state_changed\n%!" in
          (* Schedule the effect using setTimeout to ensure Bonsai can process it *)
          (* This is critical because the callback is called from JavaScript land, especially after Google redirect *)
          let effect = inject (Action.Auth_state_changed auth_state) in
          let () = Stdio.printf "Effect created, scheduling with setTimeout...\n%!" in
          let setTimeout = Js.Unsafe.global##.setTimeout in
          if Js.Optdef.test setTimeout then
            let callback_js = Js.wrap_callback (fun _ ->
              let () = Stdio.printf "*** setTimeout callback - handling Auth_state_changed effect (from Firebase callback) ***\n%!" in
              Ui_effect.Expert.handle effect;
              let () = Stdio.printf "Effect handled successfully in setTimeout callback\n%!" in
              ()) in
            ignore (Js.Unsafe.fun_call setTimeout [|
              Js.Unsafe.inject callback_js;
              Js.Unsafe.inject (Js.number_of_float 10.0) (* 10ms delay *)
            |])
          else
            (* Fallback - handle immediately if setTimeout not available *)
            let () = Stdio.printf "setTimeout not available, handling immediately\n%!" in
            Ui_effect.Expert.handle effect;
          let () = Stdio.printf "Effect scheduled successfully\n%!" in
          ()
        in
        try
          Firebase_bindings.Auth.on_auth_state_changed callback;
          let () = Stdio.printf "Firebase auth callback registered successfully\n%!" in
          Effect.Ignore
        with
        | e -> 
          let () = Stdio.printf "Error setting up auth callback in lifecycle: %s\n%!" (Exn.to_string e) in
          Effect.Ignore (* Firebase not ready yet, will retry on next activation *)
      )
      ()
  in

  let%arr model = model
  and inject = inject in
  let inject_action action = inject action in
  Components.view model inject_action
;;
