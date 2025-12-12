open! Core
open Js_of_ocaml

(* WebSocket bindings for OCaml *)

type websocket = Js.Unsafe.any

type message_event = Js.Unsafe.any

type ready_state =
  | Connecting
  | Open
  | Closing
  | Closed

let create_websocket (url : string) : websocket =
  let ws_constructor = Js.Unsafe.global##.WebSocket in
  Js.Unsafe.new_obj ws_constructor [| Js.Unsafe.inject (Js.string url) |]
;;

let send_string (ws : websocket) (message : string) : unit =
  try
    Js.Unsafe.meth_call ws "send" [| Js.Unsafe.inject (Js.string message) |]
  with
  | e -> Stdio.printf "Error sending WebSocket message: %s\n%!" (Exn.to_string e)
;;

let send_json (ws : websocket) (json_obj : (string * Js.Unsafe.any) list) : unit =
  let obj = Js.Unsafe.obj (Array.of_list json_obj) in
  let json_str = Js.Unsafe.global##.JSON##stringify obj |> Js.to_string in
  send_string ws json_str
;;

let on_open (ws : websocket) (callback : unit -> unit) : unit =
  let callback_js = Js.wrap_callback (fun _ -> callback ()) in
  Js.Unsafe.set ws (Js.string "onopen") callback_js
;;

let on_message (ws : websocket) (callback : Js.Unsafe.any -> unit) : unit =
  let callback_js =
    Js.wrap_callback (fun event ->
      try
        (* Get the data from the message event *)
        let data_str = Js.to_string (Js.Unsafe.get event (Js.string "data")) in
        (* Parse JSON *)
        let json_obj = Js.Unsafe.global##.JSON##parse (Js.string data_str) in
        callback json_obj
      with
      | e -> Stdio.printf "Error in WebSocket message callback: %s\n%!" (Exn.to_string e))
  in
  Js.Unsafe.set ws (Js.string "onmessage") callback_js
;;

let on_close (ws : websocket) (callback : unit -> unit) : unit =
  let callback_js = Js.wrap_callback (fun _ -> callback ()) in
  Js.Unsafe.set ws (Js.string "onclose") callback_js
;;

let on_error (ws : websocket) (callback : string -> unit) : unit =
  let callback_js =
    Js.wrap_callback (fun error ->
      let error_msg =
        try Js.to_string (Js.Unsafe.get error (Js.string "message")) with
        | _ -> "WebSocket error"
      in
      callback error_msg)
  in
  Js.Unsafe.set ws (Js.string "onerror") callback_js
;;

let get_ready_state (ws : websocket) : ready_state =
  let state = Js.Unsafe.get ws (Js.string "readyState") |> Js.float_of_number |> Int.of_float in
  match state with
  | 0 -> Connecting
  | 1 -> Open
  | 2 -> Closing
  | 3 -> Closed
  | _ -> Closed
;;

let is_open (ws : websocket) : bool =
  match get_ready_state ws with
  | Open -> true
  | _ -> false
;;

let close (ws : websocket) : unit =
  try Js.Unsafe.meth_call ws "close" [||] with
  | _ -> ()
;;

(* Helper to get string field from JSON object *)
let get_string_field (obj : Js.Unsafe.any) (field : string) : string option =
  try
    let value = Js.Unsafe.get obj (Js.string field) in
    if Js.Optdef.test value && not (Js.Unsafe.equals value Js.null) then
      Some (Js.to_string value)
    else
      None
  with
  | _ -> None
;;

(* Helper to get bool field from JSON object *)
let get_bool_field (obj : Js.Unsafe.any) (field : string) : bool option =
  try
    let value = Js.Unsafe.get obj (Js.string field) in
    if Js.Optdef.test value && not (Js.Unsafe.equals value Js.null) then
      Some (Js.to_bool value)
    else
      None
  with
  | _ -> None
;;
