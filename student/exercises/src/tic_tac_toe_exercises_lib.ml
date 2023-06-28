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
    
    let same_piece (pieces : Piece.t Position.Map.t) ~old_pos ~(new_pos : Position.t): bool = 
      let old_piece = Map.find pieces old_pos in
      let new_piece = Map.find pieces new_pos in 
      match old_piece, new_piece with |None, None | Some _, None | None, Some _ -> false | Some old_piece, Some new_piece -> Piece.equal old_piece new_piece ;;

    (* list of all coordinates to right to get 3 or 5 in a row *)
    let chain_dir ~offset list  =
      let added_list = List.append list [(offset (List.hd_exn (List.rev list)))] in
      added_list
    ;;

    (* evalues if a win has occured or not *)
    let evaluate ~(game_kind : Game_kind.t) ~(pieces : Piece.t Position.Map.t)
    : Evaluation.t =
    (* returns the original position's piece if every element in a list is the same piece as the Position we are checking with *)
    (* returns None if list is not a winning chain *)
    let check_for_win list = 
      let original_pos = List.hd_exn list in
      if List.for_all list ~f:(fun elem -> same_piece pieces ~old_pos:(original_pos) ~new_pos:elem) then Map.find pieces original_pos 
      else None
    in
   (* creates a list of chains of all possible locations a win could occur and returns a piece should all pieces in a chain be the same piece *)
   let check_keys_for_chain pos = 
        let all_direction_chains_for_pos = List.map Position.all_offsets ~f:(fun offset -> let chain_dir = chain_dir ~offset in Fn.apply_n_times ~n:(Game_kind.win_length game_kind - 1)chain_dir [pos]) in
        let winning_piece = List.find_map all_direction_chains_for_pos ~f:check_for_win in
        winning_piece
      in
    (* if a piece in a set has a win, we change the evaluation *)
    let winning_piece = List.find_map (Map.keys pieces) ~f:check_keys_for_chain in
    match winning_piece with | None -> Evaluation.Game_continues | Some winning_piece -> Evaluation.Game_over {winner = Some winning_piece}
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

let%expect_test "Chaining right" = (**) 
let a =  List.map Position.all_offsets ~f:(fun offset -> let chain_dir = chain_dir ~offset in Fn.apply_n_times ~n:4 chain_dir [{Position.row=0; column = 0}]) in
  (*Fn.apply_n_times ~n:4 chite [{Position.row = 0; column = 5}] in  *)
print_s [%sexp (a : Position.t list list)];
[%expect
{|
([((row 0) (column 0))]; ((row 0) (column 0))];((row 0) (column 0))];((row 0) (column 0))];((row 0) (column 0))];((row 0) (column 0))];((row 0) (column 0))];((row 0) (column 0))];((row 0) (column 0))];|}]
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
