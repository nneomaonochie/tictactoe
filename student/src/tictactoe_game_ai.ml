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
  let eval = Tic_tac_toe_exercises_lib.evaluate ~game_kind ~pieces in
  let score =
    match eval with
    | Tic_tac_toe_exercises_lib.Evaluation.Game_over { winner = Some piece }
      -> (* if my piece wins, we get positive infinity; If the other team wins we get negative infinity; Right now anything else will return 0.0 *)
      (match Piece.equal piece me with
       | true -> Float.infinity
       | false -> Float.neg_infinity)
    | _ -> 0.0
  in
  score
;;


let sample_pieces ~pieces ~me pos  = 
  let pieces = Map.set pieces ~key:pos ~data:me in
  pieces
;;

let compare_float_pos (t1 : (float, Position.t))  = 
  fun(a1, _) -> (a2, _) -> Float.compare a1 a2
;;

(* takes in a Position, a depth, and if current piece is the maximizing player to return the 
   highest score possible to anticipate which position would yield the best results*)
let rec minimax ~pos ~(pieces_old : Piece.t Position.Map.t) ~game_kind ~me ~(d : int) ~(maxPlayer : bool) = 
  (* pieces_new is the Map game_State where we decide to make the decision of going through with the given pos*)
  let pieces_new = Map.set pieces_old ~key:pos ~data:me in
  (* pos is terminal node [no children] -> game ends by a win, loss, or tie *)
  if d = 0 || (match (Tic_tac_toe_exercises_lib.evaluate ~game_kind ~pieces:pieces_new) with | Tic_tac_toe_exercises_lib.Evaluation.Game_over {winner = Some piece } -> true | _ -> false) 
    then (score ~me ~game_kind ~pieces:pieces_new, pos)
else (
  (* a list of available positions that are unfilled *)
  let available_pos = Tic_tac_toe_exercises_lib.available_moves ~game_kind ~pieces:pieces_new in
  (* a list of Map Pieces with the same indices as positions *)
  let possible_pieces = List.map available_pos ~f:(sample_pieces ~pieces:pieces_new ~me) in
  if maxPlayer then (
    let value = Float.neg_infinity in
  (* the List.map should return a list of scores for each potential position that can be placed *)
    let list_of_scores = List.map possible_pieces ~f:(fun p -> (minimax ~pieces:p ~game_kind ~me ~d:(d - 1) ~maxPlayer:false)) in
    (* this is going to pick the maximum score of the list of scores and compares it to value *)
    let max_score : float = match (List.max_elt list_of_scores ~compare:compare_float_pos) with | None -> Float.neg_infinity | Some float -> float in 
    let value = Float.max value max_score in 
    value)
  else ( (* if it is the minimizing player *)
    let value = Float.infinity in (* do i put me or Me.flip? i think still me *)
    let list_of_scores = List.map possible_pieces ~f:(fun p -> (minimax ~pieces:p ~game_kind ~me ~d:(d - 1) ~maxPlayer:true)) in
    let min_score : float = match (List.min_elt list_of_scores ~compare:Float.compare) with | None -> Float.infinity | Some float -> float in 
    let value = Float.min value min_score in 
    value
  ))
;;



(* [compute_next_move] is your Game AI's function.

   [game_ai.exe] will connect, communicate, and play with the game server,
   and will use [compute_next_move] to pick which pieces to put on your
   behalf.

   [compute_next_move] is only called whenever it is your turn, the game
   isn't yet over, so feel free to raise in cases where there are no
   available spots to pick. *)
let compute_next_move ~(me : Piece.t) ~(game_state : Game_state.t)
  : Position.t
  = (* depth is how many moves user  wants to look in the future) *)
  let pieces = game_state.pieces in
  let game_kind = game_state.game_kind in
  (* pos is a random Position of possible places AI can place its piece in *)
  (* let us try to see 3 moves in the future*)


  (* apply minimax on the children instead of at the root*)
  let pos = minimax ~pieces ~game_kind ~me ~d:3 ~maxPlayer:true


  (* let pos = (* minimax current_pos 4 true -> returns move we should go to next *)
    pick_winning_move_or_block_if_possible_strategy ~me ~game_kind ~pieces *)
  in
  pos
;;
