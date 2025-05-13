(* ================================================================ *)
(*                      TYPE DEFINITIONS (MUST BE FIRST)            *)
(* ================================================================ *)
type city = string
type route = city list
type raw_distance_data = (city * city * int) list 

module DirectCityPair = struct
  type t = string * string
  let compare = Stdlib.compare
end
module DistanceMapStd = Map.Make(DirectCityPair)
type distance_map = int DistanceMapStd.t

(* ================================================================ *)
(*                         HELPER FUNCTIONS                         *)
(* ================================================================ *)

let build_distance_map (data : raw_distance_data) : distance_map =
  List.fold_left (fun acc (c1, c2, dist) ->
    DistanceMapStd.add (c1, c2) dist acc
  ) DistanceMapStd.empty data

let get_distance (dm : distance_map) (c1 : city) (c2 : city) : int =
  if c1 = c2 then 0
  else
    try DistanceMapStd.find (c1, c2) dm
    with Not_found ->
      try DistanceMapStd.find (c2, c1) dm
      with Not_found ->
        max_int / 2 

let calculate_route_cost (r : route) (dm : distance_map) : int =
  let rec sum_segments path_segments total_cost =
      match path_segments with
      | c1 :: c2 :: rest ->
          let segment_dist = get_distance dm c1 c2 in
          if segment_dist >= (max_int / 2) then max_int / 2 
          else sum_segments (c2 :: rest) (total_cost + segment_dist)
      | [_] -> total_cost 
      | [] -> total_cost
  in
  match r with
  | [] | [_] -> 0 
  | _ -> sum_segments r 0

let print_solution (algorithm_name : string) (solution : (route * int) option) : unit =
  match solution with
  | Some (r, cost) ->
      Printf.printf "%s route: %s\n" algorithm_name (String.concat " -> " r);
      Printf.printf "Total distance: %d\n" cost
  | None -> Printf.printf "No %s solution found or error in input.\n" algorithm_name
;;

let shuffle_list (lst : 'a list) : 'a list =
  let arr = Array.of_list lst in
  let n = Array.length arr in
  for i = n - 1 downto 1 do
    let j = Random.int (i + 1) in
    let temp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- temp
  done;
  Array.to_list arr

(* ================================================================ *)
(*                      BRUTE FORCE ALGORITHM                       *)
(* ================================================================ *)

let calculate_route_cost_bf (r : route) (dm : distance_map) : int = 
  let rec sum_segments path_segments total_cost =
      match path_segments with
      | c1 :: c2 :: rest ->
          let segment_dist = get_distance dm c1 c2 in
          if segment_dist = (max_int / 2) then max_int / 2
          else sum_segments (c2 :: rest) (total_cost + segment_dist)
      | [_] -> total_cost
      | [] -> total_cost
  in
  match r with
  | [] | [_] -> 0
  | _ -> sum_segments r 0

let rec generate_permutations (lst : 'a list) : 'a list list =
    if List.length lst = 0 then [[]]
    else
      List.fold_left (fun acc elem ->
        let remaining_elements = List.filter (fun e -> e <> elem) lst in
        let perms_of_rest = generate_permutations remaining_elements in
        let new_perms = List.map (fun p -> elem :: p) perms_of_rest in
        acc @ new_perms
      ) [] lst

let solve_tsp_brute_force (all_cities : city list) (raw_distances : raw_distance_data) (start_city : city) : (route * int) option =
  if not (List.mem start_city all_cities) then
    (Printf.eprintf "Error: Start city '%s' not in the list of all cities (BruteForce).\n" start_city; None)
  else if List.length all_cities = 0 then
    None
  else
    let dm = build_distance_map raw_distances in
    let other_cities = List.filter (fun c -> c <> start_city) all_cities in
    let permutations_of_other_cities = generate_permutations other_cities in

    let possible_routes_with_costs =
      List.map (fun perm ->
        let complete_route = start_city :: perm @ [start_city] in
        let cost = calculate_route_cost_bf complete_route dm in 
        (complete_route, cost)
      ) permutations_of_other_cities
    in

    match possible_routes_with_costs with
    | [] -> None 
    | first_route_cost :: rest_routes_costs ->
        let best_route_and_cost =
          List.fold_left (fun (best_r, best_c) (curr_r, curr_c) ->
            if curr_c < best_c then (curr_r, curr_c)
            else (best_r, best_c)
          ) first_route_cost rest_routes_costs
        in
        Some best_route_and_cost

