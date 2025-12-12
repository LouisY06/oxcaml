open! Core
open! Bonsai_web
open Js_of_ocaml

(* Simple Deferred implementation using JavaScript promises - no threading required *)
module Deferred_impl = struct
  type 'a t = Js.Unsafe.any  (* JavaScript Promise *)
  
  let return (x : 'a) : 'a t =
    Js.Unsafe.fun_call (Js.Unsafe.get Js.Unsafe.global##.Promise (Js.string "resolve")) [| Js.Unsafe.inject x |]
  
  let bind (d : 'a t) ~f : 'b t =
    let f_js = Js.wrap_callback (fun x -> f x) in
    Js.Unsafe.fun_call (Js.Unsafe.get d (Js.string "then")) [| Js.Unsafe.inject f_js |]
  
  module Let_syntax = struct
    module Let_syntax = struct
      let return = return
      let bind = bind
      let map d ~f = bind d ~f:(fun x -> return (f x))
    end
  end
end

module Deferred = Deferred_impl

(* Simplified Ivar - we don't actually need it for promise_to_deferred anymore *)
(* But keep it for potential future use *)
module Ivar = struct
  type 'a t = {
    mutable resolve_fn : ('a -> unit) option;
    promise : 'a Deferred_impl.t;
  }
  
  let create () : 'a t =
    (* Create a unique ID for this promise *)
    let promise_id = Printf.sprintf "ivar_%f" (Js.Unsafe.global##.Date##now ()) in
    let resolve_key = Js.string (promise_id ^ "_resolve") in
    (* Create executor using eval to create a named function *)
    let executor_code = Printf.sprintf "(function(resolve, reject) { window.%s_resolve = resolve; })" promise_id in
    let executor = Js.Unsafe.eval_string executor_code in
    (* Create Promise *)
    let promise_constructor = Js.Unsafe.get Js.Unsafe.global##.Promise (Js.string "constructor") in
    let promise = Js.Unsafe.new_obj promise_constructor [| Js.Unsafe.inject executor |] in
    (* Store resolve function *)
    let resolve_fn = fun x ->
      try
        let resolve = Js.Unsafe.get Js.Unsafe.global resolve_key in
        ignore (Js.Unsafe.fun_call resolve [| Js.Unsafe.inject x |])
      with e ->
        let () = Stdio.printf "Error in Ivar.fill resolve: %s\n%!" (Exn.to_string e) in
        ()
    in
    { resolve_fn = Some resolve_fn; promise }
  
  let fill (ivar : 'a t) (x : 'a) : unit =
    match ivar.resolve_fn with
    | Some resolve -> resolve x
    | None -> ()
  
  let read (ivar : 'a t) : 'a Deferred.t = ivar.promise
end

(* Firebase Authentication and Firestore bindings for OCaml *)

module Auth = struct
  type user = Js.Unsafe.any
  
  type auth_state =
    | SignedOut
    | SignedIn of { uid : string; email : string option; display_name : string option }
  [@@deriving sexp, compare]
  
  let get_auth () : Js.Unsafe.any option =
    Js.Optdef.to_option (Js.Unsafe.global##.firebaseAuth)
  
  let get_current_user () : user option =
    match get_auth () with
    | None -> None
    | Some auth ->
      let current_user = Js.Unsafe.get auth (Js.string "currentUser") in
      if Js.Optdef.test current_user
      then Some current_user
      else None
  
  let get_user_info (user : user) : auth_state =
    (* Check if user is null/undefined (signed out) *)
    (* In js_of_ocaml, we check for null by comparing to Js.null *)
    try
      let () = Stdio.printf "*** get_user_info: Checking user object... ***\n%!" in
      (* Try to access uid - if it fails, user is null/undefined *)
      let uid_raw = Js.Unsafe.get user (Js.string "uid") in
      let () = Stdio.printf "*** get_user_info: Got uid_raw, checking if defined... ***\n%!" in
      let uid = 
        if Js.Optdef.test uid_raw then
          let uid_str = Js.to_string uid_raw in
          let () = Stdio.printf "*** get_user_info: uid = %s ***\n%!" uid_str in
          uid_str
        else
          let () = Stdio.printf "*** get_user_info: uid is undefined, raising exception ***\n%!" in
          raise (Failure "uid is undefined")
      in
      let email_opt =
        try
          let email = Js.Unsafe.get user (Js.string "email") in
          if Js.Optdef.test email then 
            let email_str = Js.to_string email in
            let () = Stdio.printf "*** get_user_info: email = %s ***\n%!" email_str in
            Some email_str
          else 
            let () = Stdio.printf "*** get_user_info: email is undefined ***\n%!" in
            None
        with e -> 
          let () = Stdio.printf "*** get_user_info: Exception getting email: %s ***\n%!" (Exn.to_string e) in
          None
      in
      let display_name_opt =
        try
          let name = Js.Unsafe.get user (Js.string "displayName") in
          if Js.Optdef.test name then Some (Js.to_string name) else None
        with _ -> None
      in
      let () = Stdio.printf "*** get_user_info: Returning SignedIn with uid=%s, email=%s ***\n%!" 
        uid (match email_opt with Some e -> e | None -> "None") in
      SignedIn { uid; email = email_opt; display_name = display_name_opt }
    with
    | e -> 
      let () = Stdio.printf "*** get_user_info: Exception caught: %s, returning SignedOut ***\n%!" (Exn.to_string e) in
      SignedOut (* If accessing properties fails, user is null/undefined (signed out) *)
  
  (* Helper to convert JS promise to Deferred *)
  (* Since Deferred.t is just a JavaScript Promise, we can chain it directly *)
  let promise_to_deferred (promise : Js.Unsafe.any) : 'a Deferred.t =
    (* Just return the promise - it's already a Deferred.t *)
    promise

  (* Call Firebase Auth methods - return Deferred, use Effect.Expert.handle in callers *)
  let sign_in_with_email_and_password (email : string) (password : string)
      : (user, string) Result.t Deferred.t =
    let () = Stdio.printf "*** OCAML: sign_in_with_email_and_password called with email=%s ***\n%!" email in
    let sign_in_js = Js.Unsafe.global##.firebaseSignIn in
    if Js.Optdef.test sign_in_js then
      let () = Stdio.printf "*** OCAML: Calling JavaScript firebaseSignIn function ***\n%!" in
      let promise = Js.Unsafe.fun_call sign_in_js [| Js.Unsafe.inject (Js.string email); Js.Unsafe.inject (Js.string password) |] in
      let () = Stdio.printf "*** OCAML: Promise created, binding to Deferred... ***\n%!" in
      let%bind.Deferred result = promise_to_deferred promise in
      let () = Stdio.printf "*** OCAML: Promise resolved! result=%s ***\n%!" 
        (try
          let success = Js.Unsafe.get result (Js.string "success") in
          if Js.to_bool success then "success=true" else "success=false"
        with _ -> "error reading result") in
      let success = Js.Unsafe.get result (Js.string "success") in
      if Js.to_bool success then
        let user = Js.Unsafe.get result (Js.string "user") in
        let () = Stdio.printf "*** OCAML: Sign in successful, returning Ok(user) ***\n%!" in
        Deferred.return (Ok user)
      else
        let error = Js.to_string (Js.Unsafe.get result (Js.string "error")) in
        let () = Stdio.printf "*** OCAML: Sign in failed, error=%s ***\n%!" error in
        Deferred.return (Error error)
    else
      let () = Stdio.printf "*** OCAML: Firebase not initialized! ***\n%!" in
      Deferred.return (Error "Firebase not initialized")
  
  let create_user_with_email_and_password (email : string) (password : string)
      : (user, string) Result.t Deferred.t =
    let () = Stdio.printf "*** OCAML: create_user_with_email_and_password called with email=%s ***\n%!" email in
    let create_user_js = Js.Unsafe.global##.firebaseCreateUser in
    if Js.Optdef.test create_user_js then
      let () = Stdio.printf "*** OCAML: Calling JavaScript firebaseCreateUser function ***\n%!" in
      let promise = Js.Unsafe.fun_call create_user_js [| Js.Unsafe.inject (Js.string email); Js.Unsafe.inject (Js.string password) |] in
      let () = Stdio.printf "*** OCAML: Promise created, binding to Deferred... ***\n%!" in
      let%bind.Deferred result = promise_to_deferred promise in
      let () = Stdio.printf "*** OCAML: Promise resolved! result=%s ***\n%!" 
        (try
          let success = Js.Unsafe.get result (Js.string "success") in
          if Js.to_bool success then "success=true" else "success=false"
        with _ -> "error reading result") in
      let success = Js.Unsafe.get result (Js.string "success") in
      if Js.to_bool success then
        let user = Js.Unsafe.get result (Js.string "user") in
        let () = Stdio.printf "*** OCAML: Create user successful, returning Ok(user) ***\n%!" in
        Deferred.return (Ok user)
      else
        let error = Js.to_string (Js.Unsafe.get result (Js.string "error")) in
        let () = Stdio.printf "*** OCAML: Create user FAILED with error: %s ***\n%!" error in
        Deferred.return (Error error)
    else
      let () = Stdio.printf "*** OCAML: firebaseCreateUser function not found! ***\n%!" in
      Deferred.return (Error "Firebase not initialized")
  
  let sign_in_with_google () : (user, string) Result.t Deferred.t =
    let sign_in_google_js = Js.Unsafe.global##.firebaseSignInWithGoogle in
    if Js.Optdef.test sign_in_google_js then
      let promise = Js.Unsafe.fun_call sign_in_google_js [||] in
      let%bind.Deferred result = promise_to_deferred promise in
      let success = Js.Unsafe.get result (Js.string "success") in
      if Js.to_bool success then
        (* With redirect-based auth, the page will redirect, so we return a pending state *)
        (* The actual authentication will be handled by the redirect result check on page load *)
        (* Check if there's a pending flag *)
        let pending = try Js.to_bool (Js.Unsafe.get result (Js.string "pending")) with _ -> false in
        if pending then
          (* Redirect in progress *)
          Deferred.return (Error "Redirect in progress")
        else
          (* Should not happen with redirect, but handle it *)
          let user = Js.Unsafe.get result (Js.string "user") in
          Deferred.return (Ok user)
      else
        let error = Js.to_string (Js.Unsafe.get result (Js.string "error")) in
        Deferred.return (Error error)
    else
      Deferred.return (Error "Firebase not initialized")
  
  let sign_out () : unit Deferred.t =
    let sign_out_js = Js.Unsafe.global##.firebaseSignOut in
    if Js.Optdef.test sign_out_js then
      let promise = Js.Unsafe.fun_call sign_out_js [||] in
      let%bind.Deferred _ = promise_to_deferred promise in
      Deferred.return ()
    else
      Deferred.return ()
  
  let on_auth_state_changed (callback : auth_state -> unit) : unit =
    try
      let callback_js =
        Js.wrap_callback (fun user ->
          let () = Stdio.printf "Firebase auth callback fired, user: %s\n%!" 
            (match Js.Optdef.to_option user with
             | None -> "None (signed out)"
             | Some u -> 
               let email = try Js.to_string (Js.Unsafe.get u (Js.string "email")) with _ -> "no email" in
               Printf.sprintf "Some (%s)" email) in
          let auth_state = match Js.Optdef.to_option user with
          | None -> SignedOut
          | Some u -> 
            (* get_user_info will handle null users with try/catch *)
            get_user_info u
          in
          let () = Stdio.printf "Calling OCaml callback with auth_state: %s\n%!" 
            (match auth_state with
             | SignedOut -> "SignedOut"
             | SignedIn { email; _ } -> Printf.sprintf "SignedIn (%s)" (Option.value email ~default:"no email"))
          in
          (* Schedule the callback using setTimeout to ensure Bonsai can process it *)
          (* This is especially important after Google redirect when the page reloads *)
          let setTimeout = Js.Unsafe.global##.setTimeout in
          if Js.Optdef.test setTimeout then
            ignore (Js.Unsafe.fun_call setTimeout [|
              Js.Unsafe.inject (Js.wrap_callback (fun _ ->
                let () = Stdio.printf "*** setTimeout callback - calling OCaml auth callback ***\n%!" in
                callback auth_state;
                let () = Stdio.printf "*** OCaml auth callback called successfully ***\n%!" in
                ()));
              Js.Unsafe.inject (Js.number_of_float 10.0) (* 10ms delay *)
            |])
          else
            (* Fallback - call immediately if setTimeout not available *)
            callback auth_state)
      in
      let set_callback = Js.Unsafe.global##.setFirebaseAuthCallback in
      if Js.Optdef.test set_callback then
        let () = Stdio.printf "Calling setFirebaseAuthCallback\n%!" in
        ignore (Js.Unsafe.fun_call set_callback [| Js.Unsafe.inject callback_js |])
      else
        let () = Stdio.printf "setFirebaseAuthCallback not available yet\n%!" in
        () (* Firebase not ready yet, will be called again later *)
    with
    | e -> 
      let () = Stdio.printf "Error in on_auth_state_changed: %s\n%!" (Exn.to_string e) in
      () (* Firebase not initialized, will be called again later *)
end

module Firestore = struct
  type document_reference = Js.Unsafe.any
  type collection_reference = Js.Unsafe.any
  type query_snapshot = Js.Unsafe.any
  type document_snapshot = Js.Unsafe.any
  type unsubscribe = Js.Unsafe.any
  
  (* Helper to convert OCaml values to Firestore-compatible JS values *)
  let string_to_js (s : string) : Js.Unsafe.any = Js.Unsafe.inject (Js.string s)
  let int_to_js (n : int) : Js.Unsafe.any = Js.Unsafe.inject (Js.number_of_float (Float.of_int n))
  let bool_to_js (b : bool) : Js.Unsafe.any = Js.Unsafe.inject (Js.bool b)
  
  (* Helper to convert JS promise to Deferred - same as Auth module *)
  let promise_to_deferred (promise : Js.Unsafe.any) : 'a Deferred.t =
    (* Just return the promise directly - it's already a Deferred.t *)
    promise

  (* Set document data - simplified wrapper *)
  let set_doc ?(merge = false) (collection_path : string) (doc_id : string) (data : (string * Js.Unsafe.any) list) : unit Deferred.t =
    let set_doc_js = Js.Unsafe.global##.firebaseSetDoc in
    if Js.Optdef.test set_doc_js then
      let data_obj = Js.Unsafe.obj (Array.of_list (List.map data ~f:(fun (k, v) -> (k, (v :> Js.Unsafe.any))))) in
      let promise = Js.Unsafe.fun_call set_doc_js [| Js.Unsafe.inject (Js.string collection_path); Js.Unsafe.inject (Js.string doc_id); Js.Unsafe.inject data_obj; Js.Unsafe.inject (Js.bool merge) |] in
      let%bind.Deferred _ = promise_to_deferred promise in
      Deferred.return ()
    else
      Deferred.return ()
  
  (* Get document data *)
  let get_doc (collection_path : string) (doc_id : string) : (Js.Unsafe.any option, string) Result.t Deferred.t =
    let get_doc_js = Js.Unsafe.global##.firebaseGetDoc in
    if Js.Optdef.test get_doc_js then
      let promise = Js.Unsafe.fun_call get_doc_js [| Js.Unsafe.inject (Js.string collection_path); Js.Unsafe.inject (Js.string doc_id) |] in
      let%bind.Deferred result = promise_to_deferred promise in
      let success = Js.Unsafe.get result (Js.string "success") in
      if Js.to_bool success then
        let data = Js.Unsafe.get result (Js.string "data") in
        let data_opt = if Js.Optdef.test data then Some data else None in
        Deferred.return (Ok data_opt)
      else
        let error = Js.to_string (Js.Unsafe.get result (Js.string "error")) in
        Deferred.return (Error error)
    else
      Deferred.return (Error "Firebase not initialized")
  
  (* Listen to document changes *)
  let on_snapshot (collection_path : string) (doc_id : string) (callback : Js.Unsafe.any option -> unit) : unsubscribe option =
    let callback_js = Js.wrap_callback callback in
    let on_snap_js = Js.Unsafe.global##.firebaseOnSnapshot in
    if Js.Optdef.test on_snap_js then
      Some (Js.Unsafe.fun_call on_snap_js [| Js.Unsafe.inject (Js.string collection_path); Js.Unsafe.inject (Js.string doc_id); Js.Unsafe.inject callback_js |])
    else
      None
  
  (* Query collection *)
  let query_collection (collection_path : string) (field : string) (op : string) (value : Js.Unsafe.any) : (Js.Unsafe.any list, string) Result.t Deferred.t =
    let query_js = Js.Unsafe.global##.firebaseQueryCollection in
    if Js.Optdef.test query_js then
      let promise = Js.Unsafe.fun_call query_js [| Js.Unsafe.inject (Js.string collection_path); Js.Unsafe.inject (Js.string field); Js.Unsafe.inject (Js.string op); Js.Unsafe.inject value |] in
      let%bind.Deferred result = promise_to_deferred promise in
      let success = Js.Unsafe.get result (Js.string "success") in
      if Js.to_bool success then
        let docs = Js.Unsafe.get result (Js.string "docs") in
        let docs_array = Js.to_array docs in
        let docs_list = Array.to_list (Array.map docs_array ~f:(fun d -> d)) in
        Deferred.return (Ok docs_list)
      else
        let error = Js.to_string (Js.Unsafe.get result (Js.string "error")) in
        Deferred.return (Error error)
    else
      Deferred.return (Error "Firebase not initialized")
  
  (* Delete document *)
  let delete_doc (collection_path : string) (doc_id : string) : unit Deferred.t =
    let delete_doc_js = Js.Unsafe.global##.firebaseDeleteDoc in
    if Js.Optdef.test delete_doc_js then
      let promise = Js.Unsafe.fun_call delete_doc_js [| Js.Unsafe.inject (Js.string collection_path); Js.Unsafe.inject (Js.string doc_id) |] in
      let%bind.Deferred _ = promise_to_deferred promise in
      Deferred.return ()
    else
      Deferred.return ()
end

