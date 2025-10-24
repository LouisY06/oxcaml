open! Core
open Hw1

val ai_choose_move
  :  Game_state.t
  -> strategy:[ `Greedy | `AlphaBeta of int | `Hybrid | `Adaptive ]
  -> Move.t option

val test_ai_strategies : unit -> unit
