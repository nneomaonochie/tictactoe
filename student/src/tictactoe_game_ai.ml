open! Core
open Tic_tac_toe_2023_common
open Protocol

(* Exercise 1.2.

   Implement a game AI that just picks a random available position. Feel free
   to raise ~an error~ if there is not an available position.

   After you are done, update [compute_next_move] to use your
   [random_move_strategy]. *)
let random_move_strategy
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t
  =
  let available_pos =
    Tic_tac_toe_exercises_lib.available_moves ~game_kind ~pieces
  in
  List.random_element_exn available_pos
;;

(* Exercise 3.2.

   Implement a game AI that picks a random position, unless there is an
   available winning move.

   After you are done, update [compute_next_move] to use your
   [pick_winning_move_if_possible_strategy]. *)
(* let pick_winning_move_if_possible_strategy ~(me : Piece.t) ~(game_kind :
   Game_kind.t) ~(pieces : Piece.t Position.Map.t) : Position.t = let
   winning_pos = Tic_tac_toe_exercises_lib.winning_moves ~me ~game_kind
   ~pieces in if List.is_empty winning_pos then
   pick_winning_move_or_block_if_possible_strategy ~me ~game_kind ~pieces
   else List.random_element_exn winning_pos ;; *)

(* Exercise 4.2.

   Implement a game AI that picks a random position, unless there is an
   available winning move.

   After you are done, update [compute_next_move] to use your
   [pick_winning_move_if_possible_strategy]. *)
let pick_winning_move_or_block_if_possible_strategy
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t
  =
  let winning_pos =
    Tic_tac_toe_exercises_lib.winning_moves ~me ~game_kind ~pieces
  in
  if not (List.is_empty winning_pos)
  then List.random_element_exn winning_pos
  else (
    (* the case that there are no moves that can help current piece win in
       one more *)
    (* blocking_pos is the list of Positions that will cause the opponent to
       lose *)
    let blocking_pos =
      Tic_tac_toe_exercises_lib.winning_moves
        ~me:(Piece.flip me)
        ~game_kind
        ~pieces
    in
    (* if there are no moves that block an opponents' win then pick a random
       move *)
    if not (List.is_empty blocking_pos)
    then List.random_element_exn blocking_pos
    else random_move_strategy ~game_kind ~pieces)
;;

let score
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : float
  =
  ignore me;
  ignore game_kind;
  ignore pieces;
  0.0
;;

let _ = score

(* [compute_next_move] is your Game AI's function.

   [game_ai.exe] will connect, communicate, and play with the game server,
   and will use [compute_next_move] to pick which pieces to put on your
   behalf.

   [compute_next_move] is only called whenever it is your turn, the game
   isn't yet over, so feel free to raise in cases where there are no
   available spots to pick. *)
let compute_next_move ~(me : Piece.t) ~(game_state : Game_state.t)
  : Position.t
  =
  let pieces = game_state.pieces in
  let game_kind = game_state.game_kind in
  (* pos is a random Position of possible places AI can place its piece in *)
  let pos =
    pick_winning_move_or_block_if_possible_strategy ~me ~game_kind ~pieces
  in
  pos
;;
