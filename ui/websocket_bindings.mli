open! Core
open Js_of_ocaml

(* WebSocket bindings for OCaml *)

type websocket
type message_event

type ready_state =
  | Connecting
  | Open
  | Closing
  | Closed

(* Create a new WebSocket connection *)
val create_websocket : string -> websocket

(* Send a string message *)
val send_string : websocket -> string -> unit

(* Send a JSON object *)
val send_json : websocket -> (string * Js.Unsafe.any) list -> unit

(* Set event handlers *)
val on_open : websocket -> (unit -> unit) -> unit
val on_message : websocket -> (Js.Unsafe.any -> unit) -> unit
val on_close : websocket -> (unit -> unit) -> unit
val on_error : websocket -> (string -> unit) -> unit

(* WebSocket state *)
val get_ready_state : websocket -> ready_state
val is_open : websocket -> bool
val close : websocket -> unit

(* JSON helpers *)
val get_string_field : Js.Unsafe.any -> string -> string option
val get_bool_field : Js.Unsafe.any -> bool -> bool option
