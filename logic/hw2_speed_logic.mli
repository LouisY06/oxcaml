open! Core

(* Speed card game logic interface *)
(* This module contains the core game logic interface for the Speed card game *)

module Card : sig
  type suit = Hearts | Diamonds | Clubs | Spades
  [@@deriving sexp, compare, equal]

  type rank = 
    | Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
    | Jack | Queen | King
  [@@deriving sexp, compare, equal]

  type t = { suit : suit; rank : rank }
  [@@deriving sexp, compare, equal]

  val rank_value : rank -> int
  val can_play_on : t -> t option -> bool
  val to_string : t -> string
end

module Player : sig
  type t = 
    | Player1 
    | Player2
  [@@deriving sexp, compare, equal]

  val opposite : t -> t
end

module Move : sig
  type t =
    | Play_card of { card : Card.t; pile : int }  (* pile: 0 or 1 *)
    | Draw_cards  (* Draw from stock pile *)
  [@@deriving sexp, compare]
end

module Game_state : sig
  type t = {
    (* Player hands - each player has 5 cards in hand *)
    player1_hand : Card.t list;
    player2_hand : Card.t list;
    
    (* Central piles - 2 piles where cards are played *)
    pile1 : Card.t option;  (* Top card of pile 1 *)
    pile2 : Card.t option;  (* Top card of pile 2 *)
    
    (* Stock piles - each player has a stock pile to draw from *)
    player1_stock : Card.t list;
    player2_stock : Card.t list;
    
    (* Current player *)
    current_player : Player.t;
    
    (* Game status *)
    game_over : bool;
    winner : Player.t option;
  }
  [@@deriving sexp, compare, equal]

  module Move_error : sig
    type t =
      | Game_is_over
      | Not_your_turn
      | Card_not_in_hand
      | Invalid_play  (* Card cannot be played on the specified pile *)
      | Empty_pile    (* Trying to play on empty pile *)
      | No_cards_to_draw
    [@@deriving sexp, compare]
  end

  val create : unit -> t
  val make_move : t -> Move.t -> (t, Move_error.t) Result.t
  val get_all_moves : t -> Move.t list
  val to_string : t -> string
end

module Enhanced_game_state : sig
  type t = {
    base_state : Game_state.t;
    game_log : string list;
    simultaneous_mode : bool;
    ai_thinking : bool;
    stuck_check_interval : bool;
  }
  [@@deriving sexp, compare, equal]

  val create : unit -> t
  val make_move : t -> Move.t -> string -> (t, string) Result.t
  val get_all_moves : t -> string -> Move.t list
  val are_both_players_stuck : t -> bool
  val refresh_center_cards : t -> t
  val ai_choose_move : t -> Move.t option
  val to_string : t -> string
end

(* Example values for testing *)
val initial_state : Game_state.t
val example_move : Move.t
val example_draw_move : Move.t
val test_game : unit -> unit