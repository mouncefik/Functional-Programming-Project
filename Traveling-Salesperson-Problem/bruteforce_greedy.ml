(* TSP Solver: Brute Force, Greedy, and Genetic Algorithms in OCaml *)

(* ================================================================ *)
(*                      TYPE DEFINITIONS                            *)
(* ================================================================ *)
type city = string
type route = city list (* A list of cities in the order they are visited, including return to start *)
type raw_distance_data = (city * city * int) list (* (city1, city2, distance) *)

(* Map module for distances *)
module DirectCityPair = struct
  type t = string * string
  let compare = Stdlib.compare
end
module DistanceMapStd = Map.Make(DirectCityPair)
type distance_map = int DistanceMapStd.t


(* ================================================================ *)
(*                         COMMON HELPER FUNCTIONS                  *)
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
    (Printf.eprintf "Error (BF): Start city '%s' not in the list of all cities.\n" start_city; None)
  else if List.length all_cities = 0 then 
    None
  else
    let dm = build_distance_map raw_distances in
    let other_cities = List.filter (fun c -> c <> start_city) all_cities in
    let permutations_of_other_cities = generate_permutations other_cities in
    
    let possible_routes_with_costs =
      List.map (fun perm ->
        let complete_route = start_city :: perm @ [start_city] in
        let cost = calculate_route_cost complete_route dm in
        (complete_route, cost)
      ) permutations_of_other_cities
    in
    
    match possible_routes_with_costs with
    | [] -> 
        if List.length all_cities = 1 then Some([start_city; start_city], 0)
        else None 
    | first_route_cost :: rest_routes_costs ->
        let best_route_and_cost =
          List.fold_left (fun (best_r, best_c) (curr_r, curr_c) ->
            if curr_c < best_c then (curr_r, curr_c)
            else (best_r, best_c)
          ) first_route_cost rest_routes_costs
        in
        Some best_route_and_cost

(* ================================================================ *)
(*                        GREEDY ALGORITHM                          *)
(* ================================================================ *)

let find_nearest_unvisited (current_city : city) (unvisited : city list) (dm : distance_map) : (city * int) option =
  match unvisited with
  | [] -> None
  | first_potential_city :: rest_potential_unvisited ->
      let initial_best = (first_potential_city, get_distance dm current_city first_potential_city) in
      List.fold_left (fun (best_c, best_d) next_c ->
        let dist_to_next = get_distance dm current_city next_c in
        if dist_to_next < best_d then (next_c, dist_to_next)
        else (best_c, best_d)
      ) initial_best rest_potential_unvisited |> Option.some


let solve_tsp_greedy (all_cities : city list) (raw_distances : raw_distance_data) (start_city : city) : (route * int) option =
  if not (List.mem start_city all_cities) then
    (Printf.eprintf "Error (Greedy): Start city '%s' not in the list of all cities.\n" start_city; None)
  else if List.length all_cities = 0 then
    None
  else
    let dm = build_distance_map raw_distances in
    let rec build_greedy_route_recursive current_c unvisited_c current_route_acc current_cost_acc =
      match unvisited_c with
      | [] -> 
          let cost_back_to_start = get_distance dm current_c start_city in
          if cost_back_to_start >= (max_int / 2) then
            (current_route_acc @ [start_city], max_int / 2)
          else
            (current_route_acc @ [start_city], current_cost_acc + cost_back_to_start)
      | _ ->
          match find_nearest_unvisited current_c unvisited_c dm with
          | None -> 
              Printf.eprintf "Critical Error (Greedy): Stuck at city %s with non-empty unvisited cities: %s. This indicates a logic flaw or impossible graph state not caught earlier.\n"
                current_c (String.concat ", " unvisited_c);
              (current_route_acc @ [start_city], max_int / 2)

          | Some (next_c, cost_to_next_c) ->
              if cost_to_next_c >= (max_int / 2) then
                (Printf.eprintf "Warning (Greedy): Stuck at %s. Cannot reach any of %s. Path will be incomplete or very high cost.\n"
                   current_c (String.concat ", " unvisited_c);
                 let cost_back_to_start = get_distance dm current_c start_city in
                 (current_route_acc @ [start_city], current_cost_acc + cost_to_next_c + cost_back_to_start))
              else
                let new_unvisited = List.filter (fun c -> c <> next_c) unvisited_c in
                build_greedy_route_recursive next_c new_unvisited (current_route_acc @ [next_c]) (current_cost_acc + cost_to_next_c)
    in
    if List.length all_cities = 1 then
      Some ([start_city; start_city], 0)
    else
      let other_cities = List.filter (fun c -> c <> start_city) all_cities in
      let initial_route = [start_city] in
      let initial_cost = 0 in
      let (final_route, final_cost) =
        build_greedy_route_recursive start_city other_cities initial_route initial_cost
      in
      if final_cost >= (max_int / 2) then
          (Printf.eprintf "Warning (Greedy): Algorithm resulted in a broken or very high-cost path for start %s.\n" start_city;
           None) 
      else
        Some (final_route, final_cost)


(* ================================================================ *)
(*                         MAIN PROGRAM                             *)
(* ================================================================ *)

let main () =
  Random.self_init ();

(* --- Test Case 1: Moroccan Cities (12 cities) --- *)
Printf.printf "--- TSP Solver Test with Moroccan Cities (12 cities) ---\n";
let moroccan_cities = [
  "Rabat"; "Casablanca"; "Marrakech"; "Fes"; "Tangier"; "Agadir";
  "Meknes"; "Ouarzazate"; "Essaouira"; "Tetouan"; "Chefchaouen"; "El Jadida"
] in

let raw_distances_morocco = [
  (* Distances from Rabat *)
  ("Rabat", "Casablanca", 90); ("Rabat", "Marrakech", 320); ("Rabat", "Fes", 210);
  ("Rabat", "Tangier", 250); ("Rabat", "Agadir", 520); ("Rabat", "Meknes", 130);
  ("Rabat", "Ouarzazate", 570); ("Rabat", "Essaouira", 260); ("Rabat", "Tetouan", 340);
  ("Rabat", "Chefchaouen", 380); ("Rabat", "El Jadida", 140);

  (* Distances from Casablanca *)
  ("Casablanca", "Marrakech", 240); ("Casablanca", "Fes", 290);
  ("Casablanca", "Tangier", 330); ("Casablanca", "Agadir", 460); ("Casablanca", "Meknes", 220);
  ("Casablanca", "Ouarzazate", 650); ("Casablanca", "Essaouira", 200); ("Casablanca", "Tetouan", 420);
  ("Casablanca", "Chefchaouen", 460); ("Casablanca", "El Jadida", 100);

  (* Distances from Marrakech *)
  ("Marrakech", "Fes", 530); ("Marrakech", "Tangier", 570); ("Marrakech", "Agadir", 250);
  ("Marrakech", "Meknes", 480); ("Marrakech", "Ouarzazate", 210); ("Marrakech", "Essaouira", 180);
  ("Marrakech", "Tetouan", 600); ("Marrakech", "Chefchaouen", 640); ("Marrakech", "El Jadida", 330);

  (* Distances from Fes *)
  ("Fes", "Tangier", 300); ("Fes", "Agadir", 730); ("Fes", "Meknes", 60);
  ("Fes", "Ouarzazate", 510); ("Fes", "Essaouira", 520); ("Fes", "Tetouan", 380);
  ("Fes", "Chefchaouen", 420); ("Fes", "El Jadida", 310);

  (* Distances from Tangier *)
  ("Tangier", "Agadir", 800); ("Tangier", "Meknes", 360); ("Tangier", "Ouarzazate", 810);
  ("Tangier", "Essaouira", 670); ("Tangier", "Tetouan", 90); ("Tangier", "Chefchaouen", 130);
  ("Tangier", "El Jadida", 720);

  (* Distances from Agadir *)
  ("Agadir", "Meknes", 730); ("Agadir", "Ouarzazate", 460); ("Agadir", "Essaouira", 290);
  ("Agadir", "Tetouan", 880); ("Agadir", "Chefchaouen", 920); ("Agadir", "El Jadida", 580);

  (* Distances from Meknes *)
  ("Meknes", "Ouarzazate", 490); ("Meknes", "Essaouira", 510); ("Meknes", "Tetouan", 400);
  ("Meknes", "Chefchaouen", 440); ("Meknes", "El Jadida", 300);

  (* Distances from Ouarzazate *)
  ("Ouarzazate", "Essaouira", 500); ("Ouarzazate", "Tetouan", 830); ("Ouarzazate", "Chefchaouen", 870);
  ("Ouarzazate", "El Jadida", 600);

  (* Distances from Essaouira *)
  ("Essaouira", "Tetouan", 710); ("Essaouira", "Chefchaouen", 750); ("Essaouira", "El Jadida", 370);

  (* Distances from Tetouan *)
  ("Tetouan", "Chefchaouen", 40); ("Tetouan", "El Jadida", 760);

  (* Distances from Chefchaouen *)
  ("Chefchaouen", "El Jadida", 800)
] in

let start_city_morocco = "Rabat" in

Printf.printf "All cities: %s\n" (String.concat ", " moroccan_cities);
Printf.printf "Start city: %s\n\n" start_city_morocco;



Printf.printf "Running Greedy Solver (Moroccan Cities - 12 cities)...\n";
let greedy_morocco_solution = solve_tsp_greedy moroccan_cities raw_distances_morocco start_city_morocco in
print_solution "Greedy (Moroccan)" greedy_morocco_solution;
Printf.printf "\n";

  Printf.printf "--- All tests completed. ---\n";
  ()
;;

let () = main ()