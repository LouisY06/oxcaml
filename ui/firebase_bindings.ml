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

module Ivar = struct
  type 'a t = {
    mutable value : 'a option;
    mutable resolve_fn : ('a -> unit) option;
    promise : 'a Deferred_impl.t;
  }
  
  let create () : 'a t =
    let resolve_ref = ref None in
    let promise_constructor = Js.Unsafe.get Js.Unsafe.global##.Promise (Js.string "constructor") in
    let executor = Js.wrap_callback (fun resolve _reject ->
        resolve_ref := Some (fun x -> ignore (Js.Unsafe.fun_call resolve [| Js.Unsafe.inject x |]))
      ) in
    let promise = Js.Unsafe.fun_call promise_constructor [| Js.Unsafe.inject executor |] in
    let ivar = { value = None; resolve_fn = !resolve_ref; promise } in
    resolve_ref := Some (fun x -> match ivar.resolve_fn with Some f -> f x | None -> ());
    ivar
  
  let fill (ivar : 'a t) (x : 'a) : unit =
    match ivar.resolve_fn with
    | Some resolve -> resolve x
    | None -> ivar.value <- Some x
  
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
    let uid = Js.to_string (Js.Unsafe.get user (Js.string "uid")) in
    let email_opt =
      let email = Js.Unsafe.get user (Js.string "email") in
      if Js.Optdef.test email then Some (Js.to_string email) else None
    in
    let display_name_opt =
      let name = Js.Unsafe.get user (Js.string "displayName") in
      if Js.Optdef.test name then Some (Js.to_string name) else None
    in
    SignedIn { uid; email = email_opt; display_name = display_name_opt }
  
  (* Helper to convert JS promise to Deferred *)
  let promise_to_deferred (promise : Js.Unsafe.any) : 'a Deferred.t =
    try
      let ivar = Ivar.create () in
      let on_resolve = Js.wrap_callback (fun result ->
        try
          Ivar.fill ivar result;
          ()
        with e ->
          let () = Stdio.printf "Error in promise resolve callback: %s\n%!" (Exn.to_string e) in
          ()) in
      let on_reject = Js.wrap_callback (fun error ->
        try
          let error_msg = try Js.to_string (Js.Unsafe.get error (Js.string "message")) with _ -> "Unknown error" in
          let () = Stdio.printf "Promise rejected: %s\n%!" error_msg in
          (* For now, just fill with the error object - caller will handle *)
          Ivar.fill ivar (Js.Unsafe.inject error);
          ()
        with e ->
          let () = Stdio.printf "Error in promise reject callback: %s\n%!" (Exn.to_string e) in
          ()) in
      (try
         ignore (Js.Unsafe.fun_call (Js.Unsafe.get promise (Js.string "then")) [| Js.Unsafe.inject on_resolve; Js.Unsafe.inject on_reject |])
       with e ->
         let () = Stdio.printf "Error calling promise.then: %s\n%!" (Exn.to_string e) in
         ());
      Ivar.read ivar
    with e ->
      let () = Stdio.printf "Error in promise_to_deferred: %s\n%!" (Exn.to_string e) in
      (* Return a deferred that will never resolve - this is not ideal but prevents crash *)
      let ivar = Ivar.create () in
      Ivar.read ivar

  (* Call Firebase Auth methods - return Deferred, use Effect.Expert.handle in callers *)
  let sign_in_with_email_and_password (email : string) (password : string)
      : (user, string) Result.t Deferred.t =
    let sign_in_js = Js.Unsafe.global##.firebaseSignIn in
    if Js.Optdef.test sign_in_js then
      let promise = Js.Unsafe.fun_call sign_in_js [| Js.Unsafe.inject (Js.string email); Js.Unsafe.inject (Js.string password) |] in
      let%bind.Deferred result = promise_to_deferred promise in
      let success = Js.Unsafe.get result (Js.string "success") in
      if Js.to_bool success then
        let user = Js.Unsafe.get result (Js.string "user") in
        Deferred.return (Ok user)
      else
        let error = Js.to_string (Js.Unsafe.get result (Js.string "error")) in
        Deferred.return (Error error)
    else
      Deferred.return (Error "Firebase not initialized")
  
  let create_user_with_email_and_password (email : string) (password : string)
      : (user, string) Result.t Deferred.t =
    let create_user_js = Js.Unsafe.global##.firebaseCreateUser in
    if Js.Optdef.test create_user_js then
      let promise = Js.Unsafe.fun_call create_user_js [| Js.Unsafe.inject (Js.string email); Js.Unsafe.inject (Js.string password) |] in
      let%bind.Deferred result = promise_to_deferred promise in
      let success = Js.Unsafe.get result (Js.string "success") in
      if Js.to_bool success then
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
          match Js.Optdef.to_option user with
          | None -> callback SignedOut
          | Some u -> callback (get_user_info u))
      in
      let set_callback = Js.Unsafe.global##.setFirebaseAuthCallback in
      if Js.Optdef.test set_callback then
        ignore (Js.Unsafe.fun_call set_callback [| Js.Unsafe.inject callback_js |])
      else
        () (* Firebase not ready yet *)
    with
    | _ -> () (* Firebase not initialized, will be called again later *)
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
    let ivar = Ivar.create () in
    let on_resolve = Js.wrap_callback (fun result ->
      Ivar.fill ivar result;
      ()) in
    let on_reject = Js.wrap_callback (fun error ->
      let error_msg = try Js.to_string (Js.Unsafe.get error (Js.string "message")) with _ -> "Unknown error" in
      Ivar.fill ivar (Js.Unsafe.inject (Js.string error_msg));
      ()) in
    ignore (Js.Unsafe.fun_call (Js.Unsafe.get promise (Js.string "then")) [| Js.Unsafe.inject on_resolve; Js.Unsafe.inject on_reject |]);
    Ivar.read ivar

  (* Set document data - simplified wrapper *)
  let set_doc (collection_path : string) (doc_id : string) (data : (string * Js.Unsafe.any) list) : unit Deferred.t =
    let set_doc_js = Js.Unsafe.global##.firebaseSetDoc in
    if Js.Optdef.test set_doc_js then
      let data_obj = Js.Unsafe.obj (Array.of_list (List.map data ~f:(fun (k, v) -> (k, (v :> Js.Unsafe.any))))) in
      let promise = Js.Unsafe.fun_call set_doc_js [| Js.Unsafe.inject (Js.string collection_path); Js.Unsafe.inject (Js.string doc_id); Js.Unsafe.inject data_obj |] in
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

