open Core
open Tic_tac_toe_2023_common
open Protocol

module Evaluation = struct
  type t =
    | Illegal_state
    | Game_over of { winner : Piece.t option }
    | Game_continues
  [@@deriving sexp_of]

  let to_string (t : t) = t |> sexp_of_t |> Sexp.to_string
end

(* Here are some functions which know how to create a couple different kinds
   of games *)
let empty_game =
  let game_id = Game_id.of_int 0 in
  let game_kind = Game_kind.Tic_tac_toe in
  let player_x = Player.Player (Username.of_string "Player_X") in
  let player_o = Player.Player (Username.of_string "Player_O") in
  let game_status = Game_status.Turn_of Piece.X in
  { Game_state.game_id
  ; game_kind
  ; player_x
  ; player_o
  ; pieces = Position.Map.empty
  ; game_status
  }
;;

let place_piece (game : Game_state.t) ~piece ~position : Game_state.t =
  let pieces = Map.set game.pieces ~key:position ~data:piece in
  { game with pieces }
;;

let win_for_x =
  empty_game
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 1 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 2 }
;;

let non_win =
  empty_game
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
;;

(* Exercise 1.

   For instructions on implemeting this refer to the README.

   After you are done with this implementation, you can uncomment out
   "evaluate" test cases found below in this file. *)

(* takes in a gamekind (3x3 vs 15x15), and pieces (X vs O and positions on
   Map), and returns a list of availible positions *)
let available_moves
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t list
  =
  (* piece (X/O) maps to Positon on a map (r,c)*)
  let all_pos = Game_state.get_starting_board game_kind in
  let is_occupied pos = not (Map.mem pieces pos) in
  List.filter all_pos ~f:is_occupied
;;

(* Exercise 2.

   For instructions on implemeting this refer to the README.

   After you are done with this implementation, you can uncomment out
   "evaluate" test cases found below in this file. *)

  (* checks if new Position is filled on board and is the same piece as piece we are comparing it to *)
    let same_piece ~(pieces : Piece.t Position.Map.t) ~(old_pos:Position.t) ~(new_pos): bool = 
      let old_piece = Map.find pieces old_pos in
      let new_piece = Map.find pieces new_pos in 
      match old_piece, new_piece with |None, None | Some _, None | None, Some _ -> false | Some old_piece, Some new_piece -> Piece.equal old_piece new_piece ;;

(* evalues if a win has occured or not *)
   let evaluate ~(game_kind : Game_kind.t) ~(pieces : Piece.t Position.Map.t)
  : Evaluation.t =

  (* returns true if it finds a win *)
  let rec find_win ~current_count ~direction_index ~(length_to_win : int) ~current_pos = 
    (* base case - if the current count matches length to win, we have a win*)
    if current_count = length_to_win then true 
    (* if the index exceeds indecis of all_offsets, then we went through all options and we need to return false *)
    else if direction_index = List.length Position.all_offsets then false 
    else 
      (* list of directions based off of current pos*)
      let directions current_pos f : Position.t= 
        f current_pos
    in
    let new_dir = List.map Position.all_offsets ~f:(directions (current_pos)) in
    (* this is the position we are going to check the direction in *)
    let new_pos = List.nth_exn new_dir direction_index in 
    (* if this is not bounds or it is not the piece we are looking for then we want to try again *)
    if not (Position.in_bounds new_pos ~game_kind && same_piece ~pieces ~old_pos:current_pos ~new_pos:new_pos) then 
      (* if current_count = 1, we are at the original piece we were observing and will try a different direction *)
        (if current_count = 1 then find_win ~current_count:current_count ~direction_index:(direction_index + 1) ~current_pos:current_pos else false ) 
    (* if in bounds and the piece we're looking for, we keep going forward*)
      else 
        let continues = find_win ~current_count: (current_count + 1) ~direction_index ~length_to_win ~current_pos:new_pos = true in
        if current_count = 1 && continues = false then find_win ~current_count:current_count ~direction_index:(direction_index + 1) ~current_pos:current_pos else false

      in
    
(* what if instead of a list of possible directions, we had a list of tuples - Position.t * DIRECTION - that way we can ensure we always go the same direction
   
  We could do a List.exists or something similar to that where we have [(0,0, UP) (0,1, NORTH_WEST), (1,0, WEST), etc] and we apply a function to every element of  the list where we see if a CHAIN of direction
    is possible - so instead of recursing the direction and switching the direction in the call, we recurse the direction but when done recursing, we "switch" directions by going down the list
  of the stuff
  *)





    if List.exists filled_pos ~(f:find_win  ~current_count:1 ~direction_index:0 ~length_to_win ~length_to_win : game_kind.win_length )then (*change game_State*)
  Evaluation.Illegal_state;
    ;;

(* Exercise 3. *)
let winning_moves
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t list
  =
  ignore me;
  ignore game_kind;
  ignore pieces;
  failwith "Implement me!"
;;

(* Exercise 4. *)
let losing_moves
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t list
  =
  ignore me;
  ignore game_kind;
  ignore pieces;
  failwith "Implement me!"
;;

let exercise_one =
  Command.basic
    ~summary:"Exercise 1: Where can I move?"
    (let%map_open.Command () = return () in
     fun () ->
       let moves =
         available_moves
           ~game_kind:win_for_x.game_kind
           ~pieces:win_for_x.pieces
       in
       print_s [%sexp (moves : Position.t list)];
       let moves =
         available_moves ~game_kind:non_win.game_kind ~pieces:non_win.pieces
       in
       print_s [%sexp (moves : Position.t list)])
;;

let exercise_two =
  Command.basic
    ~summary:"Exercise 2: Did is the game over?"
    (let%map_open.Command () = return () in
     fun () ->
       let evaluation =
         evaluate ~game_kind:win_for_x.game_kind ~pieces:win_for_x.pieces
       in
       print_s [%sexp (evaluation : Evaluation.t)])
;;

let exercise_three =
  let piece_options =
    Piece.all |> List.map ~f:Piece.to_string |> String.concat ~sep:", "
  in
  Command.basic
    ~summary:"Exercise 3: Is there a winning move?"
    (let%map_open.Command () = return ()
     and piece =
       flag
         "piece"
         (required (Arg_type.create Piece.of_string))
         ~doc:("PIECE " ^ piece_options)
     in
     fun () ->
       let winning_moves =
         winning_moves
           ~me:piece
           ~game_kind:non_win.game_kind
           ~pieces:non_win.pieces
       in
       print_s [%sexp (winning_moves : Position.t list)];
       ())
;;

let exercise_four =
  let piece_options =
    Piece.all |> List.map ~f:Piece.to_string |> String.concat ~sep:", "
  in
  Command.basic
    ~summary:"Exercise 4: Is there a losing move?"
    (let%map_open.Command () = return ()
     and piece =
       flag
         "piece"
         (required (Arg_type.create Piece.of_string))
         ~doc:("PIECE " ^ piece_options)
     in
     fun () ->
       let losing_moves =
         losing_moves
           ~me:piece
           ~game_kind:non_win.game_kind
           ~pieces:non_win.pieces
       in
       print_s [%sexp (losing_moves : Position.t list)];
       ())
;;

let%expect_test "print_win_for_x" =
  print_endline (Game_state.to_string_hum win_for_x);
  [%expect
    {|
    ((game_id 0)(game_kind Tic_tac_toe)(player_x(Player Player_X))(player_o(Player Player_O))(game_status(Turn_of X)))
    XOX
    OOX
    OXX |}]
;;

let%expect_test "print_non_win" =
  print_endline (Game_state.to_string_hum non_win);
  [%expect
    {|
    ((game_id 0)(game_kind Tic_tac_toe)(player_x(Player Player_X))(player_o(Player Player_O))(game_status(Turn_of X)))
    X
    O
    O X |}]
;;

(* After you've implemented [available_moves], uncomment these tests! *)
let%expect_test "yes available_moves" =
  let (moves : Position.t list) =
    available_moves ~game_kind:non_win.game_kind ~pieces:non_win.pieces
    |> List.sort ~compare:Position.compare
  in
  print_s [%sexp (moves : Position.t list)];
  [%expect
    {|
   (((row 0) (column 1)) ((row 0) (column 2)) ((row 1) (column 1))
    ((row 1) (column 2)) ((row 2) (column 1))) |}]
;;

let%expect_test "no available_moves" =
  let (moves : Position.t list) =
    available_moves ~game_kind:win_for_x.game_kind ~pieces:win_for_x.pieces
    |> List.sort ~compare:Position.compare
  in
  print_s [%sexp (moves : Position.t list)];
  [%expect {| () |}]
;;

(* When you've implemented the [evaluate] function, uncomment the next two
   tests! *)
(* let%expect_test "evalulate_win_for_x" = print_endline (evaluate
   ~game_kind:win_for_x.game_kind ~pieces:win_for_x.pieces |>
   Evaluation.to_string); [%expect {| (Win (X)) |}] ;;

   let%expect_test "evalulate_non_win" = print_endline (evaluate
   ~game_kind:non_win.game_kind ~pieces:non_win.pieces |>
   Evaluation.to_string); [%expect {| Game_continues |}] ;; *)

(* When you've implemented the [winning_moves] function, uncomment this
   test! *)
(*let%expect_test "winning_move" = let positions = winning_moves
  ~game_kind:non_win.game_kind ~pieces:non_win.pieces ~me:Piece.X in print_s
  [%sexp (positions : Position.t list)]; [%expect {| ((((row 1) (column 1))))
  |}]; let positions = winning_moves ~game_kind:non_win.game_kind
  ~pieces:non_win.pieces ~me:Piece.O in print_s [%sexp (positions :
  Position.t list)]; [%expect {| () |}] ;;*)

(* When you've implemented the [losing_moves] function, uncomment this
   test! *)
(* let%expect_test "print_losing" = let positions = losing_moves
   ~game_kind:non_win.game_kind ~pieces:non_win.pieces ~me:Piece.X in print_s
   [%sexp (positions : Position.t list)]; [%expect {| () |}]; let positions =
   losing_moves ~game_kind:non_win.game_kind ~pieces:non_win.pieces
   ~me:Piece.O in print_s [%sexp (positions : Position.t list)]; [%expect {|
   ((((row 0) (column 1)) ((row 0) (column 2)) ((row 1) (column 2)) ((row 2)
   (column 1)))) |}] ;; *)
