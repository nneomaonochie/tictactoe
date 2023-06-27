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

(* first - how to create positions second - how to create 2d array of
   positions third- List.filter to separate the ones *)

(* Map function - val keys : ('k, _, _) t -> 'k list (key is 'k, values is
   'v, and cmp can be ignored) *)

(* Exercise 2.

   For instructions on implemeting this refer to the README.

   After you are done with this implementation, you can uncomment out
   "evaluate" test cases found below in this file. *)
let evaluate ~(game_kind : Game_kind.t) ~(pieces : Piece.t Position.Map.t)
  : Evaluation.t
  =
  (* pieces has a list of ALL pieces that are on the board -> we will iterate
     the list and see if we have as many as we have in a row [(0,0) -> O;
     (1,1) -> X; (2,2) -> O] List.init

     Map.find -> for key given, it reveals the value associated with it
     Map.mem (key) - checks if a key exists in the map 
      
      
      STRATEGY:

      Jose suggests I follow the gif where i check one, then check 4 in a row consecutively


      1. using all_pos, check each index -> check if index is a KEY for pieces
          - if not a key, it dont have a piece on it so nothing to evaluate
          - if it IS a key, it is a Piece -> store if X or O, then iterate if you can find TTT with it
              - ASSUMING ALL_POS IS ORDERED LIKE MATRIX, keep checking right until length_to_win is met, 
              then down and diagonal (like my connect four or 4Queens)
                - if you iterate and get length_to_win, Game is over and change game_state
                - if you run into another piece or empty cell, then you did not win, continue down all_pos list
                  - if you reach last value in all_pos list, you're done, give up and Game_Continues
      
      
      *)
  (* This is the list of all positions that are possible on a board *)
  let all_positions = Game_state.get_starting_board game_kind in
  (* list of functions *)

  (* checks if new Position is filled on board and is the same piece as old piece *)
  let same_piece ~pieces ~old_pos:Position.t ~new_pos = 
    let piece_to_compare = Map.find pieces old_pos in
    if Map.mem pieces new_pos && Map.find pieces new_pos = piece_to_compare
      then true
    else false
;

  (* returns true if it finds a win *)
  let rec find_win ~current_count ~direction_index ~(length_to_win : int) ~current_pos: Position.t = (* how do we account for the piece with jsut the position? *)
    (* base case - if the current count matches length to win, we have a win*)
    if current_count = length_to_win then true 
    else if direction_index = List.length all_offsets then false (* this needs to keep bubbling up and up and up*)
    else  (* all_offsets = [N, NW, W, SW, S, SE, E, NE]*)
    
      let directions = all_offsets current_pos in (* list of directions based off of current pos*)
      let position_to_check = List.nth directions direction_index in (* this is the position we are going to check the direction in *)
      if not (Position.in_bounds position_to_check && same_piece ~pieces ~old_pos:current_pos ~new_pos:position_to_check) then 
          let go_in_new_direction = find_win ~current_count: current_count ~direction_index:direction_index + 1 ~current_pos:current_pos in (* this is where i stopped as of 12:30p 6/27*)
      else let continue_in_directon = find_win ~current_count: current_count + 1 ~direction_index ~length_to_win ~current_pos:new_pos

;





    (* we want current_pos to be (1,1) -> we want to up all_offsets[0] on it and see if (0,1) is in pieces, and THEN if it maps to the same value (could do this before, worry about it later)
      if up_pos is in pieces and maps to same list, count++ then recurse again with new_pos being current_pos with the same direction 
      direction index lets me know which of the directions to try at a time
      
      *)
    
      (* for (int i = 0; i < all_offsets.length; i++) {
      all_offsets[i] current_pos

    }  
      *)


      
  (* go through list of Direction function -> if Posisiont is in pieces AND maps to the same piece, then check again with the SAME function
     
    bubble up to the OG position by returning false if the call in direction is false AND count > 1 *)
      (* filled_pos is a list of position that have pieces on them; EX: (1,1); (2,2); (0,1); (0,0) *)
  let filled_pos = Map.keys pieces in 
  (* if there is a position that we can find a win in, then we find a win *)
    if List.exists filled_pos ~(f:find_win  ~current_count:1 ~direction_index:0 ~length_to_win ~length_to_win : game_kind.win_length )then (* change game_State *)
  
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
