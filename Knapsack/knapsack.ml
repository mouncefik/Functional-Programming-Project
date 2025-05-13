(* ================================================================ *)
(*       OCaml Knapsack Problem - Top-Down DP with Memoization      *)
(* ================================================================ *)

type item = {
  weight : int;
  value  : int;
}

let solve_knapsack (items_list : item list) (capacity : int) : int =

  let items_arr = Array.of_list items_list in
  let num_items = Array.length items_arr in
  let memo = Hashtbl.create (num_items * capacity) in


  let rec knapsack_recursive (index : int) (current_capacity : int) : int =
    if index = num_items then
      0
    else if current_capacity <= 0 then
      0
    else
      match Hashtbl.find_opt memo (index, current_capacity) with
      | Some precomputed_value -> precomputed_value
      | None ->
          let current_item = items_arr.(index) in
          let value_if_excluded = knapsack_recursive (index + 1) current_capacity in
          let value_if_included =
            if current_item.weight <= current_capacity then
              current_item.value + knapsack_recursive (index + 1) (current_capacity - current_item.weight)
            else
              0 
          in

          let max_value_for_state = max value_if_excluded value_if_included in
          Hashtbl.add memo (index, current_capacity) max_value_for_state;
          max_value_for_state
  in

  knapsack_recursive 0 capacity

let () =
  let items1 = [
    {weight=10; value=60};
    {weight=20; value=100};
    {weight=30; value=120}
  ] in
  let capacity1 = 50 in
  let max_value1 = solve_knapsack items1 capacity1 in
  Printf.printf "Items1, Capacity: %d, Max Value: %d\n" capacity1 max_value1; 

  let items2 = [
    {weight=5; value=10};
    {weight=4; value=40};
    {weight=6; value=30};
    {weight=3; value=50}
  ] in
  let capacity2 = 10 in
  let max_value2 = solve_knapsack items2 capacity2 in
  Printf.printf "Items2, Capacity: %d, Max Value: %d\n" capacity2 max_value2; 

  let items3 = [
    {weight=1; value=1};
    {weight=2; value=6};
    {weight=5; value=18};
    {weight=6; value=22};
    {weight=7; value=28}
  ] in
  let capacity3 = 11 in
  let max_value3 = solve_knapsack items3 capacity3 in
  Printf.printf "Items3, Capacity: %d, Max Value: %d\n" capacity3 max_value3; 

  let items4 = [{weight=10; value=100}] in
  let capacity4 = 5 in
  let max_value4 = solve_knapsack items4 capacity4 in
  Printf.printf "Items4 (item too heavy), Capacity: %d, Max Value: %d\n" capacity4 max_value4; 

  let items5 = [] in
  let capacity5 = 10 in
  let max_value5 = solve_knapsack items5 capacity5 in
  Printf.printf "Items5 (empty list), Capacity: %d, Max Value: %d\n" capacity5 max_value5; 
;;