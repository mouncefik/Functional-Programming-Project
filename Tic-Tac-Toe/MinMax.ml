
(* ================================================================ *)
(*           Tic-Tac-Toe Step 2: MinMax Implementation              *)
(* ================================================================ *)

(* Game Modeling *)
type player = X | O
type cell = player option
type board = cell list list

let initial_board () : board =
  [ [None; None; None];
    [None; None; None];
    [None; None; None] ]

let string_of_player = function X -> "X" | O -> "O"
let string_of_cell = function None -> " " | Some p -> string_of_player p

(* Display board without row/column labels *)
let display_board (b : board) : unit =
  List.iteri (fun i row ->
    let row_str = String.concat " | " (List.map string_of_cell row) in
    Printf.printf " %s\n" row_str;
    if i < 2 then print_endline "---+---+---"
  ) b;
  print_newline ()

let is_cell_empty (b : board) (r : int) (c : int) : bool =
  if r < 0 || r > 2 || c < 0 || c > 2 then false
  else List.nth (List.nth b r) c = None


(* --- Étape 2: Minimax Implementation --- *)

let opponent_of = function X -> O | O -> X

let make_move (b : board) (r : int) (c : int) (p : player) : board option =
  if r < 0 || r > 2 || c < 0 || c > 2 || not (is_cell_empty b r c) then None
  else Some (
    List.mapi (fun i_row row_data ->
      if i_row = r then
        List.mapi (fun i_col _ ->
          if i_col = c then Some p
          else List.nth row_data i_col
        ) row_data
      else row_data
    ) b
  )


let check_line (c1 : cell) (c2 : cell) (c3 : cell) (p : player) : bool =
  c1 = Some p && c2 = Some p && c3 = Some p


let check_win (b : board) (p : player) : bool =
  check_line (List.nth (List.nth b 0) 0) (List.nth (List.nth b 0) 1) (List.nth (List.nth b 0) 2) p ||
  check_line (List.nth (List.nth b 1) 0) (List.nth (List.nth b 1) 1) (List.nth (List.nth b 1) 2) p ||
  check_line (List.nth (List.nth b 2) 0) (List.nth (List.nth b 2) 1) (List.nth (List.nth b 2) 2) p ||
  check_line (List.nth (List.nth b 0) 0) (List.nth (List.nth b 1) 0) (List.nth (List.nth b 2) 0) p ||
  check_line (List.nth (List.nth b 0) 1) (List.nth (List.nth b 1) 1) (List.nth (List.nth b 2) 1) p ||
  check_line (List.nth (List.nth b 0) 2) (List.nth (List.nth b 1) 2) (List.nth (List.nth b 2) 2) p ||
  check_line (List.nth (List.nth b 0) 0) (List.nth (List.nth b 1) 1) (List.nth (List.nth b 2) 2) p ||
  check_line (List.nth (List.nth b 0) 2) (List.nth (List.nth b 1) 1) (List.nth (List.nth b 2) 0) p


let is_board_full (b : board) : bool =
  List.for_all (List.for_all (fun cell -> cell <> None)) b

(* Get all empty positions *)
let available_moves (b : board) : (int * int) list =
  let moves = ref [] in
  for r = 0 to 2 do
    for c = 0 to 2 do
      if is_cell_empty b r c then
        moves := (r, c) :: !moves
    done
  done;
  List.rev !moves


let evaluate_state (current_board : board) (ai_player_token : player) : int =
  let opponent_token = opponent_of ai_player_token in
  if check_win current_board ai_player_token then 10
  else if check_win current_board opponent_token then -10
  else if is_board_full current_board then 0
  else 999 

(* Recursive Minimax algorithm *)
let rec minimax (current_board : board) (current_player_turn : player) (ai_player_token : player) : int =
  let opponent_token = opponent_of ai_player_token in
  match () with
  | _ when check_win current_board ai_player_token -> 10
  | _ when check_win current_board opponent_token -> -10
  | _ when is_board_full current_board -> 0
  | _ ->
      let possible_next_moves = available_moves current_board in
      if current_player_turn = ai_player_token then
        let best_score = ref (-1000) in
        List.iter (fun (r, c) ->
          match make_move current_board r c current_player_turn with
          | Some next_board ->
              let score = minimax next_board (opponent_of current_player_turn) ai_player_token in
              best_score := max !best_score score
          | None -> ()
        ) possible_next_moves;
        !best_score
      else
        let best_score = ref 1000 in
        List.iter (fun (r, c) ->
          match make_move current_board r c current_player_turn with
          | Some next_board ->
              let score = minimax next_board (opponent_of current_player_turn) ai_player_token in
              best_score := min !best_score score
          | None -> ()
        ) possible_next_moves;
        !best_score

(* Find the best move for AI using Minimax *)
let find_best_move (current_board : board) (ai_player_token : player) : (int * int) option =
  let possible_moves = available_moves current_board in
  if List.length possible_moves = 0 then None
  else
    let best_move_ref = ref (List.hd possible_moves) in
    let best_score_ref = ref (-1001) in
    List.iter (fun (r, c) ->
      match make_move current_board r c ai_player_token with
      | Some next_board ->
          let move_score = minimax next_board (opponent_of ai_player_token) ai_player_token in
          if move_score > !best_score_ref then
            begin
              best_score_ref := move_score;
              best_move_ref := (r, c)
            end
      | None -> ()
    ) possible_moves;
    Some !best_move_ref

(* --- Example Usage --- *)
let () =
  print_endline "\n--- Tic-Tac-Toe: Step 2 Demo (Minimax)";

  let ai_player = O in
  let human_player = X in

  let board1 = [
    [Some O; Some O; None];
    [Some X; Some X; None];
    [None;   None;   None] ]
  in
  print_endline "Board 1 (O to win):";
  display_board board1;
  match find_best_move board1 ai_player with
  | Some (r, c) -> Printf.printf "AI 'O' best move: (%d, %d) (Expected: (0,2))\n" r c
  | None -> print_endline "AI found no move."

  let board2 = [
    [Some X; Some X; None];
    [Some O; None;   None];
    [None;   None;   None] ]
  in
  print_endline "\nBoard 2 (O to block X):";
  display_board board2;
  match find_best_move board2 ai_player with
  | Some (r, c) -> Printf.printf "AI 'O' best move: (%d, %d) (Expected: (0,2))\n" r c
  | None -> print_endline "AI found no move."

  let board3 = initial_board () in
  let board3_after_x_corner =
    match make_move board3 0 0 human_player with Some b -> b | None -> board3
  in
  print_endline "\nBoard 3 (X played corner, O's turn):";
  display_board board3_after_x_corner;
  match find_best_move board3_after_x_corner ai_player with
  | Some (r, c) -> Printf.printf "AI 'O' best move: (%d, %d) (Expected: center)\n" r c
  | None -> print_endline "AI found no move."

  let board4 = [
    [Some X; None;   Some O];
    [None;   Some X; None];
    [Some O; None;   None] ]
  in
  print_endline "\nBoard 4 (O's turn):";
  display_board board4;
  match find_best_move board4 ai_player with
  | Some (r, c) -> Printf.printf "AI 'O' best move: (%d, %d) (Expected: (2,2))\n" r c
  | None -> print_endline "AI found no move."

  print_endline "\n--- End of Demo ---"
;;