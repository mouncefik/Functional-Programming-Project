type player = X | O
type cell = player option
type board = cell list list

let initial_board () : board =
  [ [None; None; None];
    [None; None; None];
    [None; None; None] ]

let string_of_player = function
  | X -> "X"
  | O -> "O"

let string_of_cell = function
  | None -> " "
  | Some p -> string_of_player p

let display_board (b : board) : unit =
  List.iteri (fun i row ->
      let row_str = String.concat " | " (List.map string_of_cell row) in
      Printf.printf " %s\n" row_str;
      if i < 2 then print_endline " ---+---+---"
    ) b;
  print_newline ()

let is_cell_empty (b : board) (r : int) (c : int) : bool =
  if r < 0 || r > 2 || c < 0 || c > 2 then false
  else List.nth (List.nth b r) c = None

let opponent_of = function
  | X -> O
  | O -> X

let make_move (b : board) (r : int) (c : int) (p : player) : board option =
  if not (is_cell_empty b r c) then None
  else
    Some (List.mapi (fun i row ->
        if i = r then List.mapi (fun j cell -> if j = c then Some p else cell) row
        else row
      ) b)

let check_line c1 c2 c3 p =
  c1 = Some p && c2 = Some p && c3 = Some p

let check_win (b : board) (p : player) : bool =
  let open List in
  check_line (nth (nth b 0) 0) (nth (nth b 0) 1) (nth (nth b 0) 2) p ||
  check_line (nth (nth b 1) 0) (nth (nth b 1) 1) (nth (nth b 1) 2) p ||
  check_line (nth (nth b 2) 0) (nth (nth b 2) 1) (nth (nth b 2) 2) p ||
  check_line (nth (nth b 0) 0) (nth (nth b 1) 0) (nth (nth b 2) 0) p ||
  check_line (nth (nth b 0) 1) (nth (nth b 1) 1) (nth (nth b 2) 1) p ||
  check_line (nth (nth b 0) 2) (nth (nth b 1) 2) (nth (nth b 2) 2) p ||
  check_line (nth (nth b 0) 0) (nth (nth b 1) 1) (nth (nth b 2) 2) p ||
  check_line (nth (nth b 0) 2) (nth (nth b 1) 1) (nth (nth b 2) 0) p

let is_board_full (b : board) : bool =
  List.for_all (List.for_all (fun c -> c <> None)) b

let available_moves (b : board) : (int * int) list =
  let moves = ref [] in
  for r = 0 to 2 do
    for c = 0 to 2 do
      if is_cell_empty b r c then moves := (r, c) :: !moves
    done
  done;
  List.rev !moves

let evaluate_state (b : board) (ai : player) : int =
  let opponent = opponent_of ai in
  if check_win b ai then 10
  else if check_win b opponent then -10
  else if is_board_full b then 0
  else 999

let neg_infinity = -1001
let pos_infinity = 1001

let rec alphabeta (b : board) (turn : player) (ai : player) (alpha : int) (beta : int) : int =
  let opponent = opponent_of ai in
  if check_win b ai || check_win b opponent || is_board_full b then
    evaluate_state b ai
  else
    let moves = available_moves b in
    if turn = ai then
      let rec max_loop ms best alpha =
        match ms with
        | [] -> best
        | (r, c) :: rest ->
            match make_move b r c turn with
            | Some nb ->
                let eval = alphabeta nb (opponent_of turn) ai alpha beta in
                let best = max best eval in
                let alpha = max alpha best in
                if beta <= alpha then best
                else max_loop rest best alpha
            | None -> max_loop rest best alpha
      in max_loop moves neg_infinity alpha
    else
      let rec min_loop ms best beta =
        match ms with
        | [] -> best
        | (r, c) :: rest ->
            match make_move b r c turn with
            | Some nb ->
                let eval = alphabeta nb (opponent_of turn) ai alpha beta in
                let best = min best eval in
                let beta = min beta best in
                if beta <= alpha then best
                else min_loop rest best beta
            | None -> min_loop rest best beta
      in min_loop moves pos_infinity beta

let find_best_move_alphabeta (b : board) (ai : player) : (int * int) option =
  let best_move = ref None in
  let best_score = ref neg_infinity in
  let moves = available_moves b in
  List.iter (fun (r, c) ->
      match make_move b r c ai with
      | Some nb ->
          let score = alphabeta nb (opponent_of ai) ai !best_score pos_infinity in
          if score > !best_score then (
            best_score := score;
            best_move := Some (r, c)
          )
      | None -> ()
    ) moves;
  !best_move

(* === Improved User Interface === *)

let read_index_input prompt : int option =
  print_string prompt;
  match int_of_string_opt (read_line ()) with
  | Some i when i >= 0 && i <= 2 -> Some i
  | _ -> None

let rec game_loop (b : board) (current : player) (ai : player) =
  display_board b;
  if check_win b (opponent_of current) then
    Printf.printf "Player %s wins!\n" (string_of_player (opponent_of current))
  else if is_board_full b then
    print_endline "It's a draw!"
  else
  if current = ai then
    match find_best_move_alphabeta b ai with
    | Some (r, c) ->
        Printf.printf "AI (%s) plays: (%d, %d)\n" (string_of_player ai) r c;
        (match make_move b r c ai with
         | Some nb -> game_loop nb (opponent_of ai) ai
         | None -> failwith "Invalid AI move")
    | None -> print_endline "AI found no move."
  else
    match read_index_input "Enter row (0-2): " with
    | Some r ->
        (match read_index_input "Enter column (0-2): " with
         | Some c ->
             (match make_move b r c current with
              | Some nb -> game_loop nb (opponent_of current) ai
              | None ->
                  print_endline "Invalid move (cell occupied). Try again.";
                  game_loop b current ai)
         | None ->
             print_endline "Invalid column input. Try again.";
             game_loop b current ai)
    | None ->
        print_endline "Invalid row input. Try again.";
        game_loop b current ai

let () =
  print_endline "\n=== Welcome to Tic-Tac-Toe ===";
  print_endline "You are X, AI is O.";
  let initial = initial_board () in
  game_loop initial X O
