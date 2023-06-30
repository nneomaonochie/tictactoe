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
  if Map.is_empty pieces && Game_kind.board_length game_kind = 3
  then { Position.row = 1; column = 1 }
  else (
    let winning_pos =
      Tic_tac_toe_exercises_lib.winning_moves ~me ~game_kind ~pieces
    in
    if not (List.is_empty winning_pos)
    then List.random_element_exn winning_pos
    else (
      (* the case that there are no moves that can help current piece win in
         one more *)
      (* blocking_pos is the list of Positions that will cause the opponent
         to lose *)
      let blocking_pos =
        Tic_tac_toe_exercises_lib.winning_moves
          ~me:(Piece.flip me)
          ~game_kind
          ~pieces
      in
      (* if there are no moves that block an opponents' win then pick a
         random move *)
      if not (List.is_empty blocking_pos)
      then List.random_element_exn blocking_pos
      else random_move_strategy ~game_kind ~pieces))
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
      ->
      (* if my piece wins, we get positive infinity; If the other team wins
         we get negative infinity; Right now anything else will return 0.0 *)
      (match Piece.equal piece me with
       | true -> Float.infinity
       | false -> Float.neg_infinity)
    | _ -> 0.0
  in
  score
;;

let compare_float_pos
  ((f1, p1) : float * Position.t)
  ((f2, p2) : float * Position.t)
  =
  ignore p1;
  ignore p2;
  Float.compare f1 f2
;;

let found_position pos avail_pos = not (Position.equal pos avail_pos)

let max_float_pos
  ((f1, p1) : float * Position.t)
  ((f2, p2) : float * Position.t)
  =
  if Float.compare f1 f2 >= 0 then f1, p1 else f2, p2
;;

let min_float_pos
  ((f1, p1) : float * Position.t)
  ((f2, p2) : float * Position.t)
  =
  if Float.compare f1 f2 <= 0 then f1, p1 else f2, p2
;;

let get_pos_floatpos ((f1, p1) : float * Position.t) =
  ignore f1;
  p1
;;

(* takes in a Position, a depth, and if current piece is the maximizing
   player to return the highest score possible to anticipate which position
   would yield the best results*)
let rec minimax
  ~pos
  ~(pieces_old : Piece.t Position.Map.t)
  ~game_kind
  ~me
  ~availible_pos
  ~(d : int)
  ~(maxPlayer : bool)
  =
  (* pieces_new is the Map game_State where we decide to make the decision of
     going through with the given pos*)
  let pieces_new = Map.set pieces_old ~key:pos ~data:me in
  (* we update the availible position list with the position that we used
     removed *)
  let available_pos = List.filter availible_pos ~f:(found_position pos) in
  (* pos is terminal node [no children] -> game ends by a win, loss, or
     tie *)
  if d = 0
     ||
     match
       Tic_tac_toe_exercises_lib.evaluate ~game_kind ~pieces:pieces_new
     with
     | Tic_tac_toe_exercises_lib.Evaluation.Game_over
         { winner = Some _piece } ->
       true
     | _ -> false
  then score ~me ~game_kind ~pieces:pieces_new, pos
  else if maxPlayer
  then (
    let value = Float.neg_infinity, pos in
    (* the List.map should return a list of scores for each potential
       position that can be placed *)
    let list_of_scores : (float * Position.t) list =
      List.map available_pos ~f:(fun p ->
        minimax
          ~pos:p
          ~pieces_old:pieces_new
          ~game_kind
          ~me
          ~availible_pos
          ~d:(d - 1)
          ~maxPlayer:false)
    in
    (* this is going to pick the maximum score of the list of scores and
       compares it to value *)
    let max_score : float * Position.t =
      match List.max_elt list_of_scores ~compare:compare_float_pos with
      | None -> Float.neg_infinity, pos
      | Some (max_score, move) -> max_score, move
    in
    let value = max_float_pos value max_score in
    value)
  else (
    (* if it is the minimizing player *)
    let value = Float.infinity, pos in
    (* do i put me or Me.flip? i think still me *)
    let list_of_scores =
      List.map available_pos ~f:(fun p ->
        minimax
          ~pos:p
          ~pieces_old:pieces_new
          ~game_kind
          ~me
          ~availible_pos
          ~d:(d - 1)
          ~maxPlayer:true)
    in
    let min_score =
      match List.min_elt list_of_scores ~compare:compare_float_pos with
      | None -> Float.infinity, pos
      | Some (min_score, move) -> min_score, move
    in
    let value = min_float_pos value min_score in
    value)
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
  =
  (* depth is how many moves user wants to look in the future) *)
  let pieces = game_state.pieces in
  let game_kind = game_state.game_kind in
  let availible_pos =
    Tic_tac_toe_exercises_lib.available_moves ~game_kind ~pieces
  in
  (* this is what we think might be the best move - we verify by passing it
     into minimax*)
  let first_pos =
    pick_winning_move_or_block_if_possible_strategy ~me ~game_kind ~pieces
  in
  (* apply minimax on the children instead of at the root*)
  let list_of_scores =
    List.map availible_pos ~f:(fun p ->
      minimax
        ~pos:p
        ~pieces_old:pieces
        ~game_kind
        ~me
        ~availible_pos
        ~d:2
        ~maxPlayer:true)
  in
  let max_score =
    match List.max_elt list_of_scores ~compare:compare_float_pos with
    | None -> Float.neg_infinity, first_pos
    | Some (max_score, move) -> max_score, move
  in
  get_pos_floatpos max_score
;;
