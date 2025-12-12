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
   
   type game_mode =
     | SinglePlayer
     | OnlineMultiplayer of { match_id : string; player_id : string; opponent_id : string }
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
      }
  [@@deriving sexp, compare, equal]

   let initial_stats = {
     wins = 0
   ; losses = 0
   ; games_played = 0
   ; win_rate = 0.0
   }

  let initial =
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
    | Go_to_mode_selection (* Go back to mode selection *)
    | Go_to_profile (* Go to profile screen *)
    | Load_player_stats (* Load player stats from Firestore *)
    | Player_stats_loaded of Model.player_stats (* Stats loaded from Firestore *)
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

let start_matchmaking (uid : string) (inject : Action.t -> unit Effect.t) : unit Deferred.t =
  let open Deferred.Let_syntax in
  (* Create a matchmaking request in Firestore *)
  let matchmaking_id = Printf.sprintf "mm_%s_%d" uid (Int.of_float (Js.Unsafe.global##.Date##now () /. 1000.0)) in
  let data = [
    ("playerId", Firebase_bindings.Firestore.string_to_js uid)
  ; ("status", Firebase_bindings.Firestore.string_to_js "waiting")
  ; ("createdAt", Firebase_bindings.Firestore.int_to_js (Int.of_float (Js.Unsafe.global##.Date##now () /. 1000.0)))
  ] in
  let%bind _ = Firebase_bindings.Firestore.set_doc "matchmaking" matchmaking_id data in
  (* Query for other waiting players *)
  let%bind waiting_players_result = 
    Firebase_bindings.Firestore.query_collection 
      "matchmaking" 
      "status" 
      "==" 
      (Firebase_bindings.Firestore.string_to_js "waiting")
  in
  match waiting_players_result with
  | Ok docs ->
    (* Find a player that's not us *)
    let opponent_opt = 
      List.find_map docs ~f:(fun doc ->
        try
          let doc_id = Js.to_string (Js.Unsafe.get doc (Js.string "id")) in
          let doc_data = Js.Unsafe.get doc (Js.string "data") in
          if Js.Optdef.test doc_data then
            let player_id = Js.to_string (Js.Unsafe.get doc_data (Js.string "playerId")) in
            (* Not our own matchmaking request and not already matched *)
            if not (String.equal player_id uid) && not (String.equal doc_id matchmaking_id) then
              Some doc
            else
              None
          else
            None
        with _ -> None
      )
    in
    (match opponent_opt with
     | Some opponent_doc ->
       (* Found an opponent! Create a match *)
       let opponent_data = Js.Unsafe.get opponent_doc (Js.string "data") in
       let opponent_id = Js.to_string (Js.Unsafe.get opponent_data (Js.string "playerId")) in
       let opponent_matchmaking_id = Js.to_string (Js.Unsafe.get opponent_doc (Js.string "id")) in
       (* Create match document *)
       let match_id = Printf.sprintf "match_%s_%s" uid opponent_id in
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
       ] in
       (* Trigger match found action - fire and forget *)
       ignore (inject (Action.Match_found { match_id; player_id = uid; opponent_id }));
       Deferred.return ()
     | None ->
       (* No opponent found yet - set up listener to watch for matches *)
       (* Set up listener on our own matchmaking document *)
       ignore (Firebase_bindings.Firestore.on_snapshot "matchmaking" matchmaking_id (fun data_opt ->
         match data_opt with
         | None -> ()
         | Some data ->
           let status = Js.to_string (Js.Unsafe.get data (Js.string "status")) in
           if String.equal status "matched" then
             let match_id = Js.to_string (Js.Unsafe.get data (Js.string "matchId")) in
             (* Get match document to find opponent *)
             ignore (Deferred.bind ~f:(function
               | Ok (Some match_data) ->
                 let player1 = Js.to_string (Js.Unsafe.get match_data (Js.string "player1")) in
                 let player2 = Js.to_string (Js.Unsafe.get match_data (Js.string "player2")) in
                 let opponent_id = if String.equal player1 uid then player2 else player1 in
                 ignore (inject (Action.Match_found { match_id; player_id = uid; opponent_id }));
                 Deferred.return ()
               | _ -> Deferred.return ()) (Firebase_bindings.Firestore.get_doc "matches" match_id))
       ));
       Deferred.return ())
  | Error err ->
    let () = Stdio.printf "Matchmaking query failed: %s\n%!" err in
    Deferred.return ()

let setup_firestore_listener (match_id : string) (player_id : string) (inject : Action.t -> unit Effect.t) : Js.Unsafe.any option =
  (* Set up real-time listener for game state changes *)
  match Firebase_bindings.Firestore.on_snapshot "matches" match_id (fun data_opt ->
    match data_opt with
    | None -> () (* Document doesn't exist *)
    | Some data ->
      let game_state_str = Js.to_string (Js.Unsafe.get data (Js.string "gameState")) in
      let last_updated = Js.to_string (Js.Unsafe.get data (Js.string "lastUpdatedBy")) in
      (* Only update if change came from opponent *)
      if not (String.equal last_updated player_id) then
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
      { model with login_email = email }
  
  | Update_login_password password ->
      { model with login_password = password }
  
  | Sign_in ->
      (* Validate email and password before attempting sign in *)
      if String.is_empty model.login_email || String.is_empty model.login_password then
        { model with game_message = "Please enter both email and password." }
      else
        (* Sign in will be handled in state machine callback with error handling *)
        { model with login_password = ""; game_message = "Signing in..." }
  
  | Sign_up ->
      (* Validate email and password before attempting sign up *)
      if String.is_empty model.login_email || String.is_empty model.login_password then
        { model with game_message = "Please enter both email and password." }
      else
        (* Sign up will be handled in state machine callback with error handling *)
        { model with login_password = ""; game_message = "Creating account..." }
  
  | Sign_in_with_google ->
      (* Google sign in will be handled in state machine callback with error handling *)
      { model with game_message = "Signing in with Google..." }
  
  | Update_login_error error_msg ->
      { model with game_message = error_msg }
  
  | Start_game ->
      { model with 
        game_started = true
      ; game_message = "Game started! Click on your card, then click on a center pile to play!"
      }
  
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
      let () = Stdio.printf "Auth_state_changed action received\n%!" in
      let new_auth_state = match auth_state with
        | Firebase_bindings.Auth.SignedOut -> 
          let () = Stdio.printf "User signed out\n%!" in
          Model.NotAuthenticated
        | Firebase_bindings.Auth.SignedIn { uid; email; display_name } ->
          let () = Stdio.printf "User signed in: %s (uid: %s)\n%!" (Option.value email ~default:"no email") uid in
          Model.Authenticated { uid; email; display_name }
      in
      (match new_auth_state with
       | Model.NotAuthenticated ->
         let () = Stdio.printf "Setting screen to LoginScreen\n%!" in
         { model with
           auth_state = new_auth_state
         ; screen = LoginScreen
         }
       | Model.Authenticated _ ->
         let () = Stdio.printf "Setting screen to ModeSelectionScreen after login\n%!" in
         { model with
           auth_state = new_auth_state
         ; screen = ModeSelectionScreen
         })
  
  | Load_player_stats ->
      (* This will be handled in the state machine callback *)
      model
  
  | Player_stats_loaded stats ->
      { model with player_stats = Some stats }
  
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
      let new_enhanced_state = Hw2_speed_logic.Enhanced_game_state.create () in
      (* Sync initial game state to Firestore *)
      ignore (Deferred.bind ~f:(fun () -> Deferred.return ()) (sync_game_state_to_firestore match_id player_id new_enhanced_state));
      { model with
        screen = GameScreen
      ; game_mode = OnlineMultiplayer { match_id; player_id; opponent_id }
      ; enhanced_state = new_enhanced_state
      ; selected_card = None
      ; matchmaking_status = "matched"
      ; game_started = false
      ; game_message = Printf.sprintf "Match found! Click 'Start Game' to begin playing against %s" opponent_id
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
              | OnlineMultiplayer { player_id; _ } -> player_id
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
                   (match model.game_mode with
                    | OnlineMultiplayer { match_id; player_id; _ } ->
                      ignore (Deferred.bind ~f:(fun () -> Deferred.return ()) (sync_game_state_to_firestore match_id player_id state_after_stuck_check));
                      final_model
                    | SinglePlayer -> final_model)
                 else
                   (match model.game_mode with
                    | OnlineMultiplayer { match_id; player_id; _ } ->
                      ignore (Deferred.bind ~f:(fun () -> Deferred.return ()) (sync_game_state_to_firestore match_id player_id state_after_stuck_check));
                      updated_model
                    | SinglePlayer -> updated_model)
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
           updated_model)
       | _ -> 
          (* Not on game screen, game not started, or multiplayer - don't run AI *)
          model)
;;

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
        ~attrs:[ Attr.create "class" "login-screen"; Attr.create "style" "display: flex; flex-direction: column; align-items: center; justify-content: center; min-height: 100vh; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);" ]
        [ Node.div
            ~attrs:[ Attr.create "class" "login-form"; Attr.create "style" "padding: 40px; border: 2px solid #ddd; border-radius: 15px; background: white; box-shadow: 0 10px 30px rgba(0,0,0,0.3); min-width: 350px;" ]
            [ Node.h1 ~attrs:[ Attr.create "style" "text-align: center; margin-bottom: 30px; color: #333;" ] [ Node.text "Speed Card Game" ]
            ; Node.h2 ~attrs:[ Attr.create "style" "text-align: center; margin-bottom: 20px; color: #666; font-size: 18px;" ] [ Node.text "Sign In / Sign Up" ]
            ; Node.div
                ~attrs:[ Attr.create "style" "margin: 15px 0;" ]
                [ Node.label ~attrs:[ Attr.create "style" "display: block; margin-bottom: 5px; font-weight: bold; color: #333;" ] [ Node.text "Email" ]
                ; Node.input
                    ~attrs:
                      [ Attr.create "type" "email"
                      ; Attr.create "value" model.login_email
                      ; Attr.create "style" "padding: 10px; width: 100%; border: 2px solid #ddd; border-radius: 5px; font-size: 14px; box-sizing: border-box;"
                      ; Attr.on_input (fun _ text -> inject (Action.Update_login_email text))
                      ]
                    ()
                ]
            ; Node.div
                ~attrs:[ Attr.create "style" "margin: 15px 0;" ]
                [ Node.label ~attrs:[ Attr.create "style" "display: block; margin-bottom: 5px; font-weight: bold; color: #333;" ] [ Node.text "Password" ]
                ; Node.input
                    ~attrs:
                      [ Attr.create "type" "password"
                      ; Attr.create "value" model.login_password
                      ; Attr.create "style" "padding: 10px; width: 100%; border: 2px solid #ddd; border-radius: 5px; font-size: 14px; box-sizing: border-box;"
                      ; Attr.on_input (fun _ text -> inject (Action.Update_login_password text))
                      ]
                    ()
                ]
            ; Node.div
                ~attrs:[ Attr.create "style" "margin: 20px 0 10px 0; display: flex; gap: 10px;" ]
                [ Node.button
                    ~attrs:
                      [ on_click (fun _ -> inject Action.Sign_in)
                      ; Attr.create "style" "flex: 1; padding: 12px; cursor: pointer; background: #4CAF50; color: white; border: none; border-radius: 5px; font-size: 16px; font-weight: bold;"
                      ]
                    [ Node.text "Sign In" ]
                ; Node.button
                    ~attrs:
                      [ on_click (fun _ -> inject Action.Sign_up)
                      ; Attr.create "style" "flex: 1; padding: 12px; cursor: pointer; background: #2196F3; color: white; border: none; border-radius: 5px; font-size: 16px; font-weight: bold;"
                      ]
                    [ Node.text "Sign Up" ]
                ]
            ; Node.div
                ~attrs:[ Attr.create "style" "margin: 15px 0; text-align: center; color: #666; font-size: 14px;" ]
                [ Node.text "or" ]
            ; Node.button
                ~attrs:
                  [ on_click (fun _ -> inject Action.Sign_in_with_google)
                  ; Attr.create "style" "width: 100%; padding: 12px; cursor: pointer; background: white; color: #333; border: 2px solid #ddd; border-radius: 5px; font-size: 16px; font-weight: bold; display: flex; align-items: center; justify-content: center; gap: 10px;"
                  ]
                [ Node.span ~attrs:[ Attr.create "style" "font-size: 20px;" ] [ Node.text "G" ]
                ; Node.text "Sign in with Google"
                ]
            ; (if not (String.is_empty model.game_message) && (String.equal model.game_message "Signing in..." || String.equal model.game_message "Creating account..." || String.equal model.game_message "Signing in with Google...") then
                Node.div ~attrs:[ Attr.create "style" "margin-top: 15px; padding: 10px; background: #e3f2fd; border-radius: 5px; text-align: center; color: #1976d2;" ] [ Node.text model.game_message ]
              else if not (String.is_empty model.game_message) then
                Node.div ~attrs:[ Attr.create "style" "margin-top: 15px; padding: 10px; background: #ffebee; border-radius: 5px; text-align: center; color: #c62828;" ] [ Node.text model.game_message ]
              else Node.div [])
            ]
        ]
   
   (* Mode selection screen *)
   let mode_selection_screen (model : Model.t) (inject : Action.t -> unit Effect.t) =
      let open Vdom in
          Node.div
        ~attrs:[ Attr.create "class" "mode-selection-screen"; Attr.create "style" "display: flex; flex-direction: column; align-items: center; justify-content: center; min-height: 100vh; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);" ]
        [ Node.div
            ~attrs:[ Attr.create "class" "mode-selection"; Attr.create "style" "padding: 40px; border: 2px solid #ddd; border-radius: 15px; background: white; box-shadow: 0 10px 30px rgba(0,0,0,0.3); min-width: 400px; text-align: center;" ]
            [ Node.h1 ~attrs:[ Attr.create "style" "margin-bottom: 30px; color: #333;" ] [ Node.text "Choose Game Mode" ]
            ; Node.div
                ~attrs:[ Attr.create "style" "margin: 20px 0;" ]
                [ Node.button
                    ~attrs:
                      [ on_click (fun _ -> inject Action.Select_single_player)
                      ; Attr.create "style" "padding: 20px 40px; cursor: pointer; background: #4CAF50; color: white; border: none; border-radius: 10px; font-size: 18px; font-weight: bold; width: 100%; margin-bottom: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);"
                      ]
                    [ Node.text "Play Against AI" ]
                ; Node.button
                    ~attrs:
                      [ on_click (fun _ -> inject Action.Select_multiplayer)
                      ; Attr.create "style" "padding: 20px 40px; cursor: pointer; background: #FF9800; color: white; border: none; border-radius: 10px; font-size: 18px; font-weight: bold; width: 100%; box-shadow: 0 4px 6px rgba(0,0,0,0.1);"
                      ]
                    [ Node.text "Play Online (Multiplayer)" ]
                ]
            ; (match model.auth_state with
               | Model.Authenticated { email; _ } ->
                 Node.div
                   ~attrs:[ Attr.create "style" "margin-top: 30px; padding: 15px; background: #f5f5f5; border-radius: 5px;" ]
                   [ Node.text (Printf.sprintf "Signed in as: %s" (Option.value email ~default:"User"))
            ; Node.button
                ~attrs:
                  [ on_click (fun _ -> inject Action.Sign_out)
                         ; Attr.create "style" "margin-left: 10px; padding: 5px 15px; cursor: pointer; background: #f44336; color: white; border: none; border-radius: 3px;"
                  ]
                [ Node.text "Sign Out" ]
            ]
               | _ -> Node.div [])
            ; (if String.equal model.matchmaking_status "searching" then
          Node.div
                  ~attrs:[ Attr.create "style" "margin-top: 20px; padding: 15px; background: #fff3e0; border-radius: 5px; color: #e65100;" ]
                   [ Node.text "Searching for opponent... "
                   ; Node.button
                       ~attrs:
                         [ on_click (fun _ -> inject Action.Cancel_matchmaking)
                        ; Attr.create "style" "margin-left: 10px; padding: 5px 15px; cursor: pointer; background: #f44336; color: white; border: none; border-radius: 3px;"
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
        | Model.LoginScreen -> Stdio.printf "RENDERING LOGIN SCREEN\n%!"
        | Model.ProfileScreen -> Stdio.printf "RENDERING PROFILE SCREEN\n%!"
        | Model.ModeSelectionScreen -> Stdio.printf "RENDERING MODE SELECTION SCREEN\n%!"
        | Model.GameScreen -> Stdio.printf "RENDERING GAME SCREEN\n%!"
      in

      (* Route to appropriate screen *)
      match model.screen with
      | Model.LoginScreen -> login_screen model inject
      | Model.ProfileScreen -> profile_screen model inject
      | Model.ModeSelectionScreen -> mode_selection_screen model inject
      | Model.GameScreen ->
      (* Game screen *)
      let matchmaking_html =
        match model.game_mode with
        | SinglePlayer -> Node.div []
        | OnlineMultiplayer { opponent_id; _ } ->
          Node.div
            ~attrs:[ Attr.create "class" "matchmaking"; Attr.create "style" "padding: 10px; background: #fff3e0; border-radius: 5px; margin-bottom: 10px;" ]
            [ Node.text (Printf.sprintf "Playing against: %s" opponent_id) ]
      in

      (* Player hand *)
      let player_hand_html =
         Node.div
            ~attrs:[ Attr.create "class" "hand"; Attr.create "id" "player1Hand" ]
            (List.map model.enhanced_state.base_state.player1_hand ~f:(fun card ->
                 card_to_html card
                    (Option.equal Card.equal model.selected_card (Some card))
                    model.game_started false ~inject))
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
  let () = Stdio.printf "INITIALIZING APP - Starting with LoginScreen\n%!" in
  let%sub model, inject =
    Bonsai.state_machine0
      (module Model)
      (module Action)
      ~default_model:
        (* Always start with initial model - login screen first *)
        (* Don't restore saved games automatically - user must sign in first *)
        (let initial = Model.initial in
         let () = Stdio.printf "Model.initial created: screen=%s\n%!" 
           (match initial.screen with
            | Model.LoginScreen -> "LoginScreen"
            | Model.ProfileScreen -> "ProfileScreen"
            | Model.ModeSelectionScreen -> "ModeSelectionScreen"
            | Model.GameScreen -> "GameScreen")
         in
         initial)
      ~apply_action:(fun ~inject ~schedule_event:_ _model action ->
        let new_model = apply_action action _model in
        (* Handle async auth operations and errors *)
        (match action with
         | Sign_in ->
           (* Only attempt sign-in if email and password are not empty *)
           if String.is_empty new_model.login_email || String.is_empty new_model.login_password then
             new_model (* Already handled validation in apply_action, just return model *)
           else
             (* Handle sign in errors *)
             let () = Stdio.printf "Sign_in action: attempting to sign in with email=%s\n%!" new_model.login_email in
             ignore (Deferred.bind ~f:(function
               | Ok _user -> 
                 let () = Stdio.printf "Sign in successful! User authenticated. Waiting for auth state callback...\n%!" in
                 (* Auth state change will be detected by onAuthStateChanged callback *)
                 Deferred.return ()
               | Error msg -> 
                 let () = Stdio.printf "Sign in failed: %s\n%!" msg in
                 ignore (inject (Action.Update_login_error msg));
                 Deferred.return ()) (Firebase_bindings.Auth.sign_in_with_email_and_password new_model.login_email new_model.login_password));
             new_model
         | Sign_up ->
           (* Only attempt sign-up if email and password are not empty *)
           if String.is_empty new_model.login_email || String.is_empty new_model.login_password then
             new_model (* Already handled validation in apply_action, just return model *)
           else
             (* Handle sign up errors *)
             let () = Stdio.printf "Sign_up action: attempting to create account with email=%s\n%!" new_model.login_email in
             ignore (Deferred.bind ~f:(function
               | Ok _user -> 
                 let () = Stdio.printf "Sign up successful! User created and authenticated. Waiting for auth state callback...\n%!" in
                 (* Auth state change will be detected by onAuthStateChanged callback *)
                 Deferred.return ()
               | Error msg -> 
                 let () = Stdio.printf "Sign up failed: %s\n%!" msg in
                 ignore (inject (Action.Update_login_error msg));
                 Deferred.return ()) (Firebase_bindings.Auth.create_user_with_email_and_password new_model.login_email new_model.login_password));
             new_model
         | Sign_in_with_google ->
           (* Handle Google sign in - uses redirect, so page will navigate away *)
           ignore (Deferred.bind ~f:(function
             | Ok _ -> Deferred.return () (* Should not happen with redirect *)
             | Error msg -> 
               (* "Redirect in progress" is expected, don't show as error *)
               if not (String.equal msg "Redirect in progress") then
                 ignore (inject (Action.Update_login_error msg));
               Deferred.return ()) (Firebase_bindings.Auth.sign_in_with_google ()));
           new_model
         | Load_player_stats ->
           (* Load player stats from Firestore *)
           (match new_model.auth_state with
            | Model.Authenticated { uid; _ } ->
              ignore (Deferred.bind ~f:(fun () -> Deferred.return ()) (load_player_stats uid inject));
              new_model
            | _ -> new_model)
         | _ -> 
        (* Set up Firestore listener when entering multiplayer mode *)
        (match action, new_model.game_mode with
         | Match_found { match_id; player_id; _ }, OnlineMultiplayer _ ->
           (match setup_firestore_listener match_id player_id inject with
            | Some unsubscribe ->
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
              (match new_model.auth_state with
               | Model.Authenticated { uid; _ } ->
                 ignore (Deferred.bind ~f:(fun () -> Deferred.return ()) (start_matchmaking uid inject));
                 new_model
               | _ -> new_model)
            | _ -> new_model)))
  in
  
  (* Set up Firebase auth state listener - use a ref to ensure it only runs once *)
  let auth_callback_setup = ref false in
  
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

  let%arr model = model
  and inject = inject in
  (* Set up auth callback once (using ref to prevent multiple setups) *)
  let () =
    if not !auth_callback_setup then
      try
        let callback auth_state =
          (* Inject auth state change action *)
          let () = Stdio.printf "Injecting Auth_state_changed action\n%!" in
          ignore (inject (Action.Auth_state_changed auth_state))
        in
        let () = Stdio.printf "Setting up Firebase auth callback\n%!" in
        Firebase_bindings.Auth.on_auth_state_changed callback;
        auth_callback_setup := true
      with
      | e -> 
        let () = Stdio.printf "Error setting up auth callback: %s\n%!" (Exn.to_string e) in
        () (* Firebase not ready yet, will retry on next render *)
  in
  let inject_action action = inject action in
  Components.view model inject_action
;;
