(* ================================================================ *)
(*               Tic-Tac-Toe Step 1: Game Modeling                  *)
(* ================================================================ *)


type player = X | O
type cell = player option
type board = cell list list

(* Create an empty 3x3 board *)
let initial_board () : board =
  [ [None; None; None];
    [None; None; None];
    [None; None; None] ]

(* Convert player to string *)
let string_of_player = function
  | X -> "X"
  | O -> "O"

(* Convert cell to string for display *)
let string_of_cell = function
  | None -> " "
  | Some p -> string_of_player p

(* Display the current board *)
let display_board (b : board) : unit =
  print_endline "  0 | 1 | 2";
  print_endline " ---+---+---";
  List.iteri (fun i row ->
    let row_str = String.concat " | " (List.map string_of_cell row) in
    Printf.printf "%d| %s\n" i row_str;
    if i < 2 then print_endline " ---+---+---"
  ) b;
  print_newline ()

(* Check if a cell is empty *)
let is_cell_empty (b : board) (r : int) (c : int) : bool =
  if r < 0 || r >= 3 || c < 0 || c >= 3 then false
  else match List.nth (List.nth b r) c with
       | None -> true
       | Some _ -> false

(* --- Example Usage --- *)
let () =
  print_endline "--- Step 1 Demo ---";

  let b = initial_board () in
  display_board b;

  print_endline (Printf.sprintf "(0,0) empty? %b" (is_cell_empty b 0 0));
  print_endline (Printf.sprintf "(1,1) empty? %b" (is_cell_empty b 1 1));

  let b' = [[Some X; None; None]; [None; None; None]; [None; None; None]] in
  display_board b';
  print_endline (Printf.sprintf "(0,0) empty? %b" (is_cell_empty b' 0 0));

  print_endline (Printf.sprintf "(3,0) empty? %b" (is_cell_empty b 3 0));
  print_endline "--- End demo ---"
;;