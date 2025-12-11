open! Core
open! Base
open Async_kernel
open Speed_logic_library
open! Bonsai
open! Bonsai.Let_syntax
open! Bonsai_web
open Js_of_ocaml

(* HW6: Speed Card Game UI using Bonsai *)
(* Simultaneous play - both players can play at any time! *)
(* Offline support with Service Worker and Local Storage *)
(* Online multiplayer support with Firebase Auth and Firestore *)

module Firebase_bindings = Firebase_bindings

module Model = struct
   type game_mode =
     | SinglePlayer
     | OnlineMultiplayer of { match_id : string; player_id : string; opponent_id : string }
   [@@deriving sexp, compare, equal]
   
   type auth_state =
     | NotAuthenticated
     | Authenticated of { uid : string; email : string option; display_name : string option }
   [@@deriving sexp, compare, equal]
   
   type t =
      { enhanced_state : Hw2_speed_logic.Enhanced_game_state.t
      ; selected_card : Hw2_speed_logic.Card.t option
      ; game_message : string
      ; auth_state : auth_state
      ; game_mode : game_mode
      ; login_email : string
      ; login_password : string
      ; show_login : bool
      ; matchmaking_status : string (* "idle" | "searching" | "matched" | "error" *)
      ; firestore_unsubscribe : (Js.Unsafe.any option [@sexp.opaque] [@compare.ignore] [@equal.ignore]) (* For cleaning up listeners *)
      }
  [@@deriving sexp, compare, equal]

   let initial =
      { enhanced_state = Hw2_speed_logic.Enhanced_game_state.create ()
      ; selected_card = None
      ; game_message = "Welcome! Click on your card, then click on a center pile to play!"
      ; auth_state = NotAuthenticated
      ; game_mode = SinglePlayer
      ; login_email = ""
      ; login_password = ""
      ; show_login = true
      ; matchmaking_status = "idle"
      ; firestore_unsubscribe = None
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
    | Sign_in
    | Sign_up
    | Sign_out
    | Auth_state_changed of Firebase_bindings.Auth.auth_state
    | Start_matchmaking
    | Cancel_matchmaking
    | Match_found of { match_id : string; player_id : string; opponent_id : string }
    | Game_state_synced of Hw2_speed_logic.Enhanced_game_state.t (* From Firestore *)
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
    try
      (* Step 1: Access browser's localStorage *)
      match Js.Optdef.to_option Dom_html.window##.localStorage with
      | None -> None (* localStorage not available *)
      | Some storage ->
        (* Step 2: Retrieve the stored string value *)
        let key = Js.string storage_key in
        match Js.Opt.to_option (storage##getItem key) with
        | None -> None (* No saved game found *)
        | Some value ->
          (* Step 3: Convert string back to S-expression *)
          let json_str = Js.to_string value in
          (* Parse the S-expression string *)
          let sexp = Parsexp.Single.parse_string_exn json_str in
          
          (* Step 4: Deserialize S-expression back to OCaml Model.t *)
          let model = Model.t_of_sexp sexp in
          
          (* Log success for debugging *)
          let () = Stdio.printf "Game loaded from local storage\n%!" in
          Some model
    with
    | _ -> 
      (* If deserialization fails (corrupted data, version mismatch, etc.) *)
      let () = Stdio.printf "Failed to load game from local storage\n%!" in
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
      (* Fire and forget async sign in *)
       ignore (Deferred.bind ~f:(function
         | Ok _ -> Deferred.return () (* Auth state will update via listener *)
         | Error msg -> let () = Stdio.printf "Sign in failed: %s\n%!" msg in Deferred.return ()) (Firebase_bindings.Auth.sign_in_with_email_and_password model.login_email model.login_password));
      { model with login_password = ""; game_message = "Signing in..." }
  
  | Sign_up ->
      (* Fire and forget async sign up *)
       ignore (Deferred.bind ~f:(function
         | Ok _ -> Deferred.return () (* Auth state will update via listener *)
         | Error msg -> let () = Stdio.printf "Sign up failed: %s\n%!" msg in Deferred.return ()) (Firebase_bindings.Auth.create_user_with_email_and_password model.login_email model.login_password));
      { model with login_password = ""; game_message = "Creating account..." }
  
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
      ; game_mode = SinglePlayer
      ; show_login = true
      ; matchmaking_status = "idle"
      ; firestore_unsubscribe = None
      ; game_message = "Signed out successfully."
      }
  
  | Auth_state_changed auth_state ->
      let new_auth_state = match auth_state with
        | Firebase_bindings.Auth.SignedOut -> Model.NotAuthenticated
        | Firebase_bindings.Auth.SignedIn { uid; email; display_name } ->
          Model.Authenticated { uid; email; display_name }
      in
      { model with
        auth_state = new_auth_state
      ; show_login = (match new_auth_state with Model.NotAuthenticated -> true | _ -> false)
      }
  
  | Start_matchmaking ->
      (match model.auth_state with
       | Model.NotAuthenticated ->
         { model with game_message = "Please sign in first to play online!" }
       | Model.Authenticated { uid; _ } ->
         (* Start matchmaking process - this will be handled via Effect *)
          ignore (Deferred.bind ~f:(fun () -> Deferred.return ()) (start_matchmaking uid (fun _action -> Effect.return ())));
         { model with matchmaking_status = "searching"; game_message = "Searching for opponent..." })
  
  | Cancel_matchmaking ->
      { model with matchmaking_status = "idle"; game_message = "Matchmaking cancelled." }
  
  | Match_found { match_id; player_id; opponent_id } ->
      let new_enhanced_state = Hw2_speed_logic.Enhanced_game_state.create () in
      (* Note: Firestore listener setup needs inject function, which we don't have here *)
      (* This will be set up separately when the match is found *)
       ignore (Deferred.bind ~f:(fun () -> Deferred.return ()) (sync_game_state_to_firestore match_id player_id new_enhanced_state));
      { model with
        game_mode = OnlineMultiplayer { match_id; player_id; opponent_id }
      ; enhanced_state = new_enhanced_state
      ; selected_card = None
      ; matchmaking_status = "matched"
      ; game_message = Printf.sprintf "Match found! Playing against %s" opponent_id
      }
  
  | Game_state_synced new_state ->
      { model with enhanced_state = new_state }
  
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
      (* Only run AI in single player mode *)
      (match model.game_mode with
       | OnlineMultiplayer _ -> model (* Opponent plays via Firestore sync *)
       | SinglePlayer ->
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

      (* Login form *)
      let login_form_html =
        if model.show_login then
          Node.div
            ~attrs:[ Attr.create "class" "login-form"; Attr.create "style" "padding: 20px; border: 2px solid #ddd; border-radius: 10px; margin-bottom: 20px; background: #f9f9f9;" ]
            [ Node.h2 [ Node.text "Sign In / Sign Up" ]
            ; Node.div
                ~attrs:[ Attr.create "style" "margin: 10px 0;" ]
                [ Node.label [ Node.text "Email: " ]
                ; Node.input
                    ~attrs:
                      [ Attr.create "type" "email"
                      ; Attr.create "value" model.login_email
                      ; Attr.create "style" "padding: 5px; margin-left: 10px; width: 200px;"
                      ; Attr.on_input (fun _ text -> inject (Action.Update_login_email text))
                      ]
                    ()
                ]
            ; Node.div
                ~attrs:[ Attr.create "style" "margin: 10px 0;" ]
                [ Node.label [ Node.text "Password: " ]
                ; Node.input
                    ~attrs:
                      [ Attr.create "type" "password"
                      ; Attr.create "value" model.login_password
                      ; Attr.create "style" "padding: 5px; margin-left: 10px; width: 200px;"
                      ; Attr.on_input (fun _ text -> inject (Action.Update_login_password text))
                      ]
                    ()
                ]
            ; Node.div
                ~attrs:[ Attr.create "style" "margin: 10px 0;" ]
                [ Node.button
                    ~attrs:
                      [ on_click (fun _ -> inject Action.Sign_in)
                      ; Attr.create "style" "padding: 10px 20px; margin-right: 10px; cursor: pointer; background: #4CAF50; color: white; border: none; border-radius: 5px;"
                      ]
                    [ Node.text "Sign In" ]
                ; Node.button
                    ~attrs:
                      [ on_click (fun _ -> inject Action.Sign_up)
                      ; Attr.create "style" "padding: 10px 20px; cursor: pointer; background: #2196F3; color: white; border: none; border-radius: 5px;"
                      ]
                    [ Node.text "Sign Up" ]
                ]
            ]
        else
          Node.div
            ~attrs:[ Attr.create "class" "user-info"; Attr.create "style" "padding: 10px; background: #e8f5e9; border-radius: 5px; margin-bottom: 10px;" ]
            [ Node.span
                ~attrs:[ Attr.create "style" "margin-right: 20px;" ]
                [ Node.text
                    (match model.auth_state with
                     | Model.NotAuthenticated -> "Not signed in"
                     | Model.Authenticated { email; display_name = _; _ } ->
                       Printf.sprintf "Signed in as: %s" (Option.value email ~default:"User"))
                ]
            ; Node.button
                ~attrs:
                  [ on_click (fun _ -> inject Action.Sign_out)
                  ; Attr.create "style" "padding: 5px 15px; cursor: pointer; background: #f44336; color: white; border: none; border-radius: 3px;"
                  ]
                [ Node.text "Sign Out" ]
            ]
      in

      (* Matchmaking controls *)
      let matchmaking_html =
        match model.auth_state with
        | Model.NotAuthenticated -> Node.div []
        | Model.Authenticated _ ->
          Node.div
            ~attrs:[ Attr.create "class" "matchmaking"; Attr.create "style" "padding: 10px; background: #fff3e0; border-radius: 5px; margin-bottom: 10px;" ]
            [ Node.div
                ~attrs:[ Attr.create "style" "margin-bottom: 10px;" ]
                [ Node.text
                    (match model.game_mode with
                     | SinglePlayer -> "Playing against AI"
                     | OnlineMultiplayer { opponent_id; _ } -> Printf.sprintf "Playing against: %s" opponent_id)
                ]
            ; (match model.matchmaking_status with
               | "searching" ->
                 Node.div
                   ~attrs:[ Attr.create "style" "margin: 10px 0;" ]
                   [ Node.text "Searching for opponent... "
                   ; Node.button
                       ~attrs:
                         [ on_click (fun _ -> inject Action.Cancel_matchmaking)
                         ; Attr.create "style" "padding: 5px 15px; cursor: pointer; background: #f44336; color: white; border: none; border-radius: 3px;"
                         ]
                       [ Node.text "Cancel" ]
                   ]
               | "matched" -> Node.div []
               | _ ->
                 Node.button
                   ~attrs:
                     [ on_click (fun _ -> inject Action.Start_matchmaking)
                     ; Attr.create "style" "padding: 10px 20px; cursor: pointer; background: #FF9800; color: white; border: none; border-radius: 5px;"
                     ]
                   [ Node.text "Find Online Opponent" ])
            ]
      in

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
         [ login_form_html
         ; matchmaking_html
         ; Node.div
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
(* FIXED Bonsai App Initialization *)
(* ================================= *)
let app =
  let%sub model, inject =
    Bonsai.state_machine0
      (module Model)
      (module Action)

(**************************************************)
      ~default_model:
        (* Try to load from local storage on startup, fallback to initial *)
        (match LocalStorage.load () with
         | Some saved_model -> 
           { saved_model with game_message = "Welcome back! Your game has been restored." }
         | None -> Model.initial)
      ~apply_action:(fun ~inject ~schedule_event:_ _model action ->
        let new_model = apply_action action _model in
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
         | _ -> new_model))
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
          ignore (inject (Action.Auth_state_changed auth_state))
        in
        Firebase_bindings.Auth.on_auth_state_changed callback;
        auth_callback_setup := true
      with
      | _ -> () (* Firebase not ready yet, will retry on next render *)
  in
  let inject_action action = inject action in
  Components.view model inject_action
;;
