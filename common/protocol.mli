open! Core
open! Async_rpc_kernel

(** The protocol for connections between "players" and the Tic Tac Toe game
    server. where the "players" can "play" via either a web ui, or through
    their Game AI bots. *)

module Game_kind : sig
  type t =
    | Tic_tac_toe
    | Omok
  [@@deriving sexp, equal, bin_io]

  val to_string : t -> string
  val to_string_hum : t -> string

  (** [board_length] returns the length of the board. 3 for [ Tic_tac_toe ]
      and 15 for [Omok]. *)
  val board_length : t -> int

  (** [win_length] returns the winning length of the board. 3 for
      [ Tic_tac_toe ] and 5 for [Omok]. *)
  val win_length : t -> int
end

module Difficulty : sig
  (** This [difficulty] represents the "server bot" difficulty when you click
      on the UI. *)
  type t =
    | Easy
    | Medium
    | Hard
  [@@deriving sexp, equal, bin_io]

  val to_string : t -> string
end

module Game_id : sig
  type t

  include Identifiable.S with type t := t

  val of_int : int -> t
  val to_int : t -> int
end

module Username : sig
  type t

  include Identifiable.S with type t := t
end

module With_username : sig
  type 'a t =
    { txt : 'a
    ; username : Username.t
    }
end

module Create_game : sig
  module Query : sig
    type t =
      { game_kind : Game_kind.t
      ; against_server_bot : Difficulty.t option
      }
    [@@deriving sexp_of, equal, bin_io]
  end

  val rpc : (Query.t With_username.t, Game_id.t) Rpc.Rpc.t
end

module Join_existing_game : sig
  module Response : sig
    type t =
      | Ok
      | You've_already_joined_this_game
      | Game_does_not_exist
      | Game_already_full
      | Game_already_ended
    [@@deriving sexp_of, equal, bin_io]
  end

  val rpc : (Game_id.t With_username.t, Response.t) Rpc.Rpc.t
end

module Player : sig
  type t =
    | Server_bot of Difficulty.t
    | Player of Username.t
  [@@deriving sexp_of, equal, bin_io]
end

module Piece : sig
  type t =
    | X
    | O
  [@@deriving sexp, equal, bin_io, enumerate]

  val of_string : string -> t
  val to_string : t -> string

  (* [flip] gives you the "other" piece. | X -> O | O -> X *)
  val flip : t -> t
end

module Position : sig
  (* Top-left is [{row = 0; column = 0}].

     row indexes increment downwards.

     column indexes increment rightwards. *)

  type t =
    { row : int
    ; column : int
    }
  [@@deriving sexp_of, equal, bin_io, compare]

  val to_string : t -> string
  val in_bounds : t -> game_kind:Game_kind.t -> bool

  (** [down t] is [t]'s downwards neighbor. *)
  val down : t -> t

  (** [right t] is [t]'s rightwards neighbor. *)
  val right : t -> t

  (** [up t] is [t]'s upwards neighbor. *)
  val up : t -> t

  (** [left t] is [t]'s leftwards neighbor. *)
  val left : t -> t

  (** [all_offsets] is a list of functions to compute all 8 neighbors of a
      cell (i.e. left, up-left, up, up-right, right, right-down, down,
      down-left). *)
  val all_offsets : (t -> t) list

  include Comparable.S_plain with type t := t
end

module Game_status : sig
  type t =
    | Turn_of of Piece.t
    | Game_over of { winner : (Piece.t * Position.Set.t) option }
  [@@deriving sexp_of, equal, bin_io]
end

module Game_state : sig
  (** [Game_state.t] represents a game with two players. *)
  type t =
    { game_id : Game_id.t
    ; game_kind : Game_kind.t
    ; player_x : Player.t
    ; player_o : Player.t
    ; pieces : Piece.t Position.Map.t
    ; game_status : Game_status.t
    }
  [@@deriving sexp_of, equal, bin_io]

  val get_player : t -> piece:Piece.t -> Player.t
  val to_string_hum : t -> string
  val get_starting_board : Game_kind.t -> Position.t list
end

module Show_all_games_with_two_players : sig
  val rpc : (unit, Game_state.t Game_id.Map.t) Rpc.Rpc.t
end

module Joinable_game : sig
  type t =
    { game_id : Game_id.t
    ; game_kind : Game_kind.t
    ; player_x : Player.t
    }
  [@@deriving sexp_of, equal, bin_io]
end

module Get_game : sig
  module Response : sig
    type t =
      | Both_players of Game_state.t
      | Waiting_for_someone_to_join of Joinable_game.t
      | Game_id_does_not_exist
    [@@deriving sexp_of]
  end

  val rpc : (Game_id.t, Response.t) Rpc.Rpc.t
end

module Is_thinking : sig
  val rpc : (Game_id.t, bool) Rpc.Rpc.t
end

module List_all_joinable_games : sig
  val rpc : (unit, Joinable_game.t Game_id.Map.t) Rpc.Rpc.t
end

module Take_turn : sig
  module Query : sig
    type t =
      { game_id : Game_id.t
      ; position : Position.t
      }
    [@@deriving sexp_of, equal, bin_io]
  end

  module Response : sig
    type t =
      | Ok
      | Game_does_not_exist
      | Not_your_turn
      | Position_out_of_bounds
      | Position_already_occupied
      | Game_is_over
      | You_are_not_a_player_in_this_game
    [@@deriving sexp_of, equal, bin_io]
  end

  val rpc : (Query.t With_username.t, Response.t) Rpc.Rpc.t
end

module World_state : sig
  type t =
    { joinable_games : Joinable_game.t Game_id.Map.t
    ; running_games : Game_state.t Game_id.Map.t
    }
  [@@deriving bin_io, sexp_of, equal]

  val empty : t
end

module Me : sig
  val rpc : (unit, Username.t) Rpc.Rpc.t
end
