(* ================================================================ *)
(*                       GENETIC ALGORITHM                          *)
(* ================================================================ *)

type city = string
type route = city list
type individual = route * int
type population = individual list
type raw_distance_data = (city * city * int) list

module CityPairMap = Map.Make(struct
  type t = city * city
  let compare = compare
end)

type distance_map = int CityPairMap.t
let take n lst =
  let rec loop acc i = function
    | [] -> List.rev acc
    | hd :: tl when i >= n -> List.rev acc
    | hd :: tl -> loop (hd :: acc) (i + 1) tl
  in
  loop [] 0 lst

let shuffle_list (lst : 'a list) : 'a list =
  let arr = Array.of_list lst in
  for i = (Array.length arr) - 1 downto 1 do
    let j = Random.int (i + 1) in
    let temp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- temp
  done;
  Array.to_list arr

let build_distance_map (raw_distances : raw_distance_data) : distance_map =
  List.fold_left (fun map (c1, c2, dist) ->
    let map_with_c1_c2 = CityPairMap.add (c1, c2) dist map in
    CityPairMap.add (c2, c1) dist map_with_c1_c2 (* Add reverse for easier lookup *)
  ) CityPairMap.empty raw_distances

(* get distance between two cities from map *)
let get_distance (dm : distance_map) (c1 : city) (c2 : city) : int =
  try CityPairMap.find (c1, c2) dm
  with Not_found ->
    failwith ("No distance found for pair (" ^ c1 ^ ", " ^ c2 ^ ") - check raw_distances and build_distance_map")

(* calculate total cost of a route *)
let calculate_route_cost (route : route) (dm : distance_map) : int =
  let rec loop = function
    | [] | [_] -> 0
    | c1 :: c2 :: rest ->
        let d = get_distance dm c1 c2 in
        d + loop (c2 :: rest)
  in
  loop route


let get_permutable_part (r : route) : city list =
  match r with
  | _start_city :: middle_cities_and_end_city ->
      (try List.rev (List.tl (List.rev middle_cities_and_end_city)) with Failure _ -> []) (* Remove last element *)
  | [] -> []

let reconstruct_route (start_c : city) (perm_part : city list) : route =
  start_c :: perm_part @ [start_c]

let create_random_individual (all_cities : city list) (start_city : city) (dm : distance_map) : individual =
  let other_cities = List.filter (fun c -> c <> start_city) all_cities in
  let shuffled_others = if List.length other_cities > 0 then shuffle_list other_cities else [] in
  let route = reconstruct_route start_city shuffled_others in
  (route, calculate_route_cost route dm)

let initialize_population (pop_size : int) (all_cities : city list) (start_city : city) (dm : distance_map) : population =
  List.init pop_size (fun _ -> create_random_individual all_cities start_city dm)

let tournament_selection (population : population) (tournament_size : int) : individual =
  let rec select_k_and_find_best k current_best_opt_accrued competitors_indices =
    if k = 0 then
      match current_best_opt_accrued with
      | Some best -> best
      | None -> List.hd (List.sort (fun (_, f1) (_, f2) -> compare f1 f2) population) 
    else
      let idx = Random.int (List.length population) in
      if List.mem idx competitors_indices then 
        select_k_and_find_best k current_best_opt_accrued competitors_indices
      else
        let competitor = List.nth population idx in
        let (_, competitor_fitness) = competitor in
        match current_best_opt_accrued with
        | None ->
            select_k_and_find_best (k - 1) (Some competitor) (idx :: competitors_indices)
        | Some (_, best_fitness) ->
            if competitor_fitness < best_fitness then
              select_k_and_find_best (k - 1) (Some competitor) (idx :: competitors_indices)
            else
              select_k_and_find_best (k - 1) current_best_opt_accrued (idx :: competitors_indices)
  in
  if List.length population = 0 then failwith "Cannot select from empty population";
  let actual_tournament_size = min tournament_size (List.length population) in
  if actual_tournament_size = 0 then List.hd population 
  else select_k_and_find_best actual_tournament_size None []

let ordered_crossover (parent1_route : route) (parent2_route : route) (start_c : city) : route * route =
  let p1_perm = get_permutable_part parent1_route in
  let p2_perm = get_permutable_part parent2_route in
  let n = List.length p1_perm in

  if n <= 1 then (parent1_route, parent2_route) 
  else
    let cp1 = Random.int n in
    let cp2 = Random.int n in
    let start_idx = min cp1 cp2 in
    let end_idx = max cp1 cp2 in

    let create_child p_donor p_receiver =
      let child_perm_arr = Array.make n None in 
      let segment_cities_from_donor = ref [] in

  
      for i = start_idx to end_idx do
        let city_from_donor = List.nth p_donor i in
        child_perm_arr.(i) <- Some city_from_donor;
        segment_cities_from_donor := city_from_donor :: !segment_cities_from_donor
      done;


      let current_receiver_idx = ref 0 in
      let current_child_slot = ref ((end_idx + 1) mod n) in 
      let filled_count = ref 0 in
      let num_to_fill_from_receiver = n - (List.length !segment_cities_from_donor) in

      while !filled_count < num_to_fill_from_receiver do
        if !current_receiver_idx >= n then 
          failwith "OX1 Crossover Error: Ran out of cities in p_receiver prematurely.";

        let city_from_receiver = List.nth p_receiver !current_receiver_idx in

        if not (List.mem city_from_receiver !segment_cities_from_donor) then (
         
            if child_perm_arr.(!current_child_slot) <> None then
                failwith "OX1 Crossover Error: Attempting to fill an already occupied slot in child.";

            child_perm_arr.(!current_child_slot) <- Some city_from_receiver;
            current_child_slot := (!current_child_slot + 1) mod n;
            filled_count := !filled_count + 1
        );
        current_receiver_idx := !current_receiver_idx + 1;
      done;


      let final_child_perm = Array.to_list (Array.map (function Some c -> c | None -> failwith "OX1: Unfilled slot in child") child_perm_arr) in
      reconstruct_route start_c final_child_perm
    in

    let child1_route = create_child p1_perm p2_perm in
    let child2_route = create_child p2_perm p1_perm in
    (child1_route, child2_route)



let swap_mutation (r : route) (mutation_rate : float) (start_c : city) : route =
  if Random.float 1.0 < mutation_rate then
    let perm_part = get_permutable_part r in
    let n = List.length perm_part in
    if n < 2 then r 
    else
      let idx1 = Random.int n in
      let idx2 = Random.int n in
      if idx1 = idx2 then r  
      else
        let arr = Array.of_list perm_part in
        let temp = arr.(idx1) in
        arr.(idx1) <- arr.(idx2);
        arr.(idx2) <- temp;
        reconstruct_route start_c (Array.to_list arr)
  else
    r

(* Main GA Solver *)
let solve_tsp_ga (all_cities : city list) (raw_distances : raw_distance_data) (start_city : city)
                  (pop_size : int) (num_generations : int) (tournament_size : int)
                  (crossover_rate : float) (mutation_rate : float) (elitism_count : int) : (route * int) option =

  Random.self_init (); 

  if not (List.mem start_city all_cities) then
    (Printf.eprintf "Error (GA): Start city '%s' not in list of all cities.\n" start_city; None)
  else if List.length all_cities = 0 then
    (Printf.eprintf "Error (GA): List of all cities is empty.\n"; None)
  else if List.length all_cities = 1 && List.hd all_cities = start_city then
    Some (reconstruct_route start_city [], 0) 
  else if List.length all_cities < 2 then 
     (Printf.eprintf "Error (GA): Not enough cities to form a tour (need at least start city and one other, or just start city).\n"; None)
  else
    let dm = build_distance_map raw_distances in
    let rec run_generation gen_num current_population best_overall_solution =
      if gen_num >= num_generations then best_overall_solution
      else
        let sorted_population = List.sort (fun (_, f1) (_, f2) -> compare f1 f2) current_population in
        let elite_individuals = take elitism_count sorted_population in
        let current_gen_best_individual =
            if List.length sorted_population > 0 then List.hd sorted_population
            else failwith "Population became empty during generation."
        in
        let updated_best_overall_solution =
          match best_overall_solution with
          | None -> Some current_gen_best_individual
          | Some (_, best_overall_fitness) ->
              if snd current_gen_best_individual < best_overall_fitness then Some current_gen_best_individual
              else best_overall_solution
        in
        if gen_num mod 20 = 0 || gen_num = num_generations -1 then (
          let (current_best_route_overall, best_fitness_overall) =
            match updated_best_overall_solution with Some ind -> ind | None -> ([], max_int) in
           Printf.printf "GA Gen: %3d, Best Fitness this Gen: %6d, Best Overall Fitness: %6d, Route sample: %s...\n"
             gen_num (snd current_gen_best_individual) best_fitness_overall (String.concat "->" (take 5 current_best_route_overall));
           flush stdout;
        );

        let offspring_population = ref elite_individuals in
        let num_parents_to_select = pop_size - elitism_count in

        if num_parents_to_select > 0 then (
            for _i = 1 to (num_parents_to_select + 1) / 2 do 
              if List.length !offspring_population < pop_size then (
                  let parent1 = tournament_selection current_population tournament_size in
                  let parent2 = tournament_selection current_population tournament_size in

                  let child1_route, child2_route =
                    if Random.float 1.0 < crossover_rate then
                      ordered_crossover (fst parent1) (fst parent2) start_city
                    else (fst parent1, fst parent2) 
                  in

                  let mutated_child1_route = swap_mutation child1_route mutation_rate start_city in
                  let mutated_child2_route = swap_mutation child2_route mutation_rate start_city in

                  let child1 = (mutated_child1_route, calculate_route_cost mutated_child1_route dm) in
                  offspring_population := child1 :: !offspring_population;

                  if List.length !offspring_population < pop_size then (
                    let child2 = (mutated_child2_route, calculate_route_cost mutated_child2_route dm) in
                    offspring_population := child2 :: !offspring_population
                  )
              )
            done;
        );
        let final_offspring_population = take pop_size !offspring_population in
        run_generation (gen_num + 1) final_offspring_population updated_best_overall_solution
    in
    let initial_pop = initialize_population pop_size all_cities start_city dm in
    if List.length initial_pop = 0 then (Printf.eprintf "Error: Initial population is empty.\n"; None)
    else run_generation 0 initial_pop None

(* --- Dataset for 30 Moroccan Cities --- *)
let thirty_moroccan_cities = [
  "Casablanca"; "Marrakech"; "Fes"; "Rabat"; "Tangier"; "Meknes"; "Agadir"; "Ouarzazate";
  "Tetouan"; "Chefchaouen"; "Essaouira"; "Ifrane"; "El Jadida"; "Safi"; "Kenitra"; "Nador";
  "Oujda"; "Al Hoceima"; "Taza"; "Beni Mellal"; "Khouribga"; "Settat"; "Larache"; "Ksar El Kebir";
  "Sidi Ifni"; "Tiznit"; "Taroudant"; "Zagora"; "Errachidia"; "Midelt"
]


let raw_thirty_moroccan_distances = [
  (* Agadir connections *)
  ("Agadir", "Al Hoceima", 910); ("Agadir", "Beni Mellal", 480); ("Agadir", "Casablanca", 460);
  ("Agadir", "Chefchaouen", 780); ("Agadir", "El Jadida", 360); ("Agadir", "Errachidia", 580);
  ("Agadir", "Essaouira", 175); ("Agadir", "Fes", 730); ("Agadir", "Ifrane", 670);
  ("Agadir", "Kenitra", 590); ("Agadir", "Khouribga", 450); ("Agadir", "Ksar El Kebir", 730);
  ("Agadir", "Larache", 700); ("Agadir", "Marrakech", 250); ("Agadir", "Meknes", 670);
  ("Agadir", "Midelt", 650); ("Agadir", "Nador", 1000); ("Agadir", "Ouarzazate", 300);
  ("Agadir", "Oujda", 1050); ("Agadir", "Rabat", 530); ("Agadir", "Safi", 160);
  ("Agadir", "Settat", 400); ("Agadir", "Sidi Ifni", 160); ("Agadir", "Tangier", 780);
  ("Agadir", "Taroudant", 85); ("Agadir", "Taza", 850); ("Agadir", "Tetouan", 820);
  ("Agadir", "Tiznit", 90); ("Agadir", "Zagora", 480);

  (* Al Hoceima connections *)
  ("Al Hoceima", "Beni Mellal", 550); ("Al Hoceima", "Casablanca", 500);
  ("Al Hoceima", "Chefchaouen", 230); ("Al Hoceima", "El Jadida", 580);
  ("Al Hoceima", "Errachidia", 420); ("Al Hoceima", "Essaouira", 750);
  ("Al Hoceima", "Fes", 320); ("Al Hoceima", "Ifrane", 370);
  ("Al Hoceima", "Kenitra", 420); ("Al Hoceima", "Khouribga", 500);
  ("Al Hoceima", "Ksar El Kebir", 260); ("Al Hoceima", "Larache", 230);
  ("Al Hoceima", "Marrakech", 750); ("Al Hoceima", "Meknes", 370);
  ("Al Hoceima", "Midelt", 350); ("Al Hoceima", "Nador", 160); ("Al Hoceima", "Ouarzazate", 700);
  ("Al Hoceima", "Oujda", 520); ("Al Hoceima", "Rabat", 400); ("Al Hoceima", "Safi", 680);
  ("Al Hoceima", "Settat", 460); ("Al Hoceima", "Sidi Ifni", 980); ("Al Hoceima", "Tangier", 200);
  ("Al Hoceima", "Taroudant", 720); ("Al Hoceima", "Taza", 260); ("Al Hoceima", "Tetouan", 180);
  ("Al Hoceima", "Tiznit", 760); ("Al Hoceima", "Zagora", 820); ("Al Hoceima", "Errachidia", 420);

  (* Beni Mellal connections *)
  ("Beni Mellal", "Casablanca", 240); ("Beni Mellal", "Chefchaouen", 530);
  ("Beni Mellal", "El Jadida", 200); ("Beni Mellal", "Errachidia", 320);
  ("Beni Mellal", "Essaouira", 320); ("Beni Mellal", "Fes", 260);
  ("Beni Mellal", "Ifrane", 260); ("Beni Mellal", "Kenitra", 220);
  ("Beni Mellal", "Khouribga", 80); ("Beni Mellal", "Ksar El Kebir", 480);
  ("Beni Mellal", "Larache", 450); ("Beni Mellal", "Marrakech", 240);
  ("Beni Mellal", "Meknes", 190); ("Beni Mellal", "Midelt", 310);
  ("Beni Mellal", "Nador", 620); ("Beni Mellal", "Ouarzazate", 400);
  ("Beni Mellal", "Oujda", 700); ("Beni Mellal", "Rabat", 200);
  ("Beni Mellal", "Safi", 220); ("Beni Mellal", "Settat", 160);
  ("Beni Mellal", "Sidi Ifni", 620); ("Beni Mellal", "Tangier", 530);
  ("Beni Mellal", "Taroudant", 400); ("Beni Mellal", "Taza", 400);
  ("Beni Mellal", "Tetouan", 570); ("Beni Mellal", "Tiznit", 460);
  ("Beni Mellal", "Zagora", 550); ("Beni Mellal", "Errachidia", 320);

  (* Casablanca connections *)
  ("Casablanca", "Chefchaouen", 430); ("Casablanca", "El Jadida", 100);
  ("Casablanca", "Errachidia", 600); ("Casablanca", "Essaouira", 230);
  ("Casablanca", "Fes", 420); ("Casablanca", "Ifrane", 500);
  ("Casablanca", "Kenitra", 90); ("Casablanca", "Khouribga", 160);
  ("Casablanca", "Ksar El Kebir", 420); ("Casablanca", "Larache", 400);
  ("Casablanca", "Marrakech", 240); ("Casablanca", "Meknes", 360);
  ("Casablanca", "Midelt", 530); ("Casablanca", "Nador", 700);
  ("Casablanca", "Ouarzazate", 550); ("Casablanca", "Oujda", 850);
  ("Casablanca", "Rabat", 90); ("Casablanca", "Safi", 190);
  ("Casablanca", "Settat", 70); ("Casablanca", "Sidi Ifni", 600);
  ("Casablanca", "Tangier", 420); ("Casablanca", "Taroudant", 480);
  ("Casablanca", "Taza", 600); ("Casablanca", "Tetouan", 460);
  ("Casablanca", "Tiznit", 520); ("Casablanca", "Zagora", 650);
  ("Casablanca", "Errachidia", 600);

  (* Chefchaouen connections *)
  ("Chefchaouen", "El Jadida", 530); ("Chefchaouen", "Errachidia", 520);
  ("Chefchaouen", "Essaouira", 670); ("Chefchaouen", "Fes", 200);
  ("Chefchaouen", "Ifrane", 280); ("Chefchaouen", "Kenitra", 260);
  ("Chefchaouen", "Khouribga", 460); ("Chefchaouen", "Ksar El Kebir", 120);
  ("Chefchaouen", "Larache", 100); ("Chefchaouen", "Marrakech", 670);
  ("Chefchaouen", "Meknes", 260); ("Chefchaouen", "Midelt", 340);
  ("Chefchaouen", "Nador", 270); ("Chefchaouen", "Ouarzazate", 720);
  ("Chefchaouen", "Oujda", 640); ("Chefchaouen", "Rabat", 240);
  ("Chefchaouen", "Safi", 620); ("Chefchaouen", "Settat", 400);
  ("Chefchaouen", "Sidi Ifni", 920); ("Chefchaouen", "Tangier", 90);
  ("Chefchaouen", "Taroudant", 660); ("Chefchaouen", "Taza", 240);
  ("Chefchaouen", "Tetouan", 70); ("Chefchaouen", "Tiznit", 700);
  ("Chefchaouen", "Zagora", 840); ("Chefchaouen", "Errachidia", 520);

  (* El Jadida connections *)
  ("El Jadida", "Errachidia", 520); ("El Jadida", "Essaouira", 190);
  ("El Jadida", "Fes", 400); ("El Jadida", "Ifrane", 450);
  ("El Jadida", "Kenitra", 130); ("El Jadida", "Khouribga", 130);
  ("El Jadida", "Ksar El Kebir", 430); ("El Jadida", "Larache", 420);
  ("El Jadida", "Marrakech", 250); ("El Jadida", "Meknes", 340);
  ("El Jadida", "Midelt", 480); ("El Jadida", "Nador", 680);
  ("El Jadida", "Ouarzazate", 530); ("El Jadida", "Oujda", 830);
  ("El Jadida", "Rabat", 110); ("El Jadida", "Safi", 150);
  ("El Jadida", "Settat", 90); ("El Jadida", "Sidi Ifni", 580);
  ("El Jadida", "Tangier", 440); ("El Jadida", "Taroudant", 460);
  ("El Jadida", "Taza", 580); ("El Jadida", "Tetouan", 480);
  ("El Jadida", "Tiznit", 500); ("El Jadida", "Zagora", 630);
  ("El Jadida", "Errachidia", 520);

  (* Errachidia connections *)
  ("Errachidia", "Essaouira", 650); ("Errachidia", "Fes", 220);
  ("Errachidia", "Ifrane", 200); ("Errachidia", "Kenitra", 520);
  ("Errachidia", "Khouribga", 380); ("Errachidia", "Ksar El Kebir", 650);
  ("Errachidia", "Larache", 680); ("Errachidia", "Marrakech", 500);
  ("Errachidia", "Meknes", 200); ("Errachidia", "Midelt", 180);
  ("Errachidia", "Nador", 520); ("Errachidia", "Ouarzazate", 180);
  ("Errachidia", "Oujda", 420); ("Errachidia", "Rabat", 520);
  ("Errachidia", "Safi", 600); ("Errachidia", "Settat", 460);
  ("Errachidia", "Sidi Ifni", 800); ("Errachidia", "Tangier", 720);
  ("Errachidia", "Taroudant", 360); ("Errachidia", "Taza", 220);
  ("Errachidia", "Tetouan", 700); ("Errachidia", "Tiznit", 680);
  ("Errachidia", "Zagora", 180);

  (* Essaouira connections *)
  ("Essaouira", "Fes", 580); ("Essaouira", "Ifrane", 580);
  ("Essaouira", "Kenitra", 430); ("Essaouira", "Khouribga", 430);
  ("Essaouira", "Ksar El Kebir", 680); ("Essaouira", "Larache", 660);
  ("Essaouira", "Marrakech", 170); ("Essaouira", "Meknes", 520);
  ("Essaouira", "Midelt", 560); ("Essaouira", "Nador", 880);
  ("Essaouira", "Ouarzazate", 450); ("Essaouira", "Oujda", 980);
  ("Essaouira", "Rabat", 410); ("Essaouira", "Safi", 190);
  ("Essaouira", "Settat", 360); ("Essaouira", "Sidi Ifni", 400);
  ("Essaouira", "Tangier", 680); ("Essaouira", "Taroudant", 260);
  ("Essaouira", "Taza", 700); ("Essaouira", "Tetouan", 720);
  ("Essaouira", "Tiznit", 300); ("Essaouira", "Zagora", 630);

  (* Fes connections *)
  ("Fes", "Ifrane", 60); ("Fes", "Kenitra", 220);
  ("Fes", "Khouribga", 260); ("Fes", "Ksar El Kebir", 320);
  ("Fes", "Larache", 340); ("Fes", "Marrakech", 480);
  ("Fes", "Meknes", 60); ("Fes", "Midelt", 160);
  ("Fes", "Nador", 400); ("Fes", "Ouarzazate", 440);
  ("Fes", "Oujda", 500); ("Fes", "Rabat", 200);
  ("Fes", "Safi", 540); ("Fes", "Settat", 280);
  ("Fes", "Sidi Ifni", 880); ("Fes", "Tangier", 380);
  ("Fes", "Taroudant", 640); ("Fes", "Taza", 120);
  ("Fes", "Tetouan", 360); ("Fes", "Tiznit", 720);
  ("Fes", "Zagora", 580);

  (* Ifrane connections *)
  ("Ifrane", "Kenitra", 300); ("Ifrane", "Khouribga", 260);
  ("Ifrane", "Ksar El Kebir", 420); ("Ifrane", "Larache", 440);
  ("Ifrane", "Marrakech", 420); ("Ifrane", "Meknes", 120);
  ("Ifrane", "Midelt", 140); ("Ifrane", "Nador", 480);
  ("Ifrane", "Ouarzazate", 380); ("Ifrane", "Oujda", 560);
  ("Ifrane", "Rabat", 280); ("Ifrane", "Safi", 480);
  ("Ifrane", "Settat", 280); ("Ifrane", "Sidi Ifni", 860);
  ("Ifrane", "Tangier", 460); ("Ifrane", "Taroudant", 580);
  ("Ifrane", "Taza", 180); ("Ifrane", "Tetouan", 440);
  ("Ifrane", "Tiznit", 660); ("Ifrane", "Zagora", 520);

  (* Kenitra connections *)
  ("Kenitra", "Khouribga", 220); ("Kenitra", "Ksar El Kebir", 340);
  ("Kenitra", "Larache", 320); ("Kenitra", "Marrakech", 520);
  ("Kenitra", "Meknes", 160); ("Kenitra", "Midelt", 340);
  ("Kenitra", "Nador", 420); ("Kenitra", "Ouarzazate", 620);
  ("Kenitra", "Oujda", 660); ("Kenitra", "Rabat", 15);
  ("Kenitra", "Safi", 480); ("Kenitra", "Settat", 170);
  ("Kenitra", "Sidi Ifni", 820); ("Kenitra", "Tangier", 300);
  ("Kenitra", "Taroudant", 700); ("Kenitra", "Taza", 320);
  ("Kenitra", "Tetouan", 320); ("Kenitra", "Tiznit", 720);
  ("Kenitra", "Zagora", 720);

  (* Khouribga connections *)
  ("Khouribga", "Ksar El Kebir", 420); ("Khouribga", "Larache", 400);
  ("Khouribga", "Marrakech", 320); ("Khouribga", "Meknes", 240);
  ("Khouribga", "Midelt", 320); ("Khouribga", "Nador", 600);
  ("Khouribga", "Ouarzazate", 480); ("Khouribga", "Oujda", 680);
  ("Khouribga", "Rabat", 180); ("Khouribga", "Safi", 280);
  ("Khouribga", "Settat", 120); ("Khouribga", "Sidi Ifni", 660);
  ("Khouribga", "Tangier", 420); ("Khouribga", "Taroudant", 560);
  ("Khouribga", "Taza", 420); ("Khouribga", "Tetouan", 460);
  ("Khouribga", "Tiznit", 600); ("Khouribga", "Zagora", 640);

  (* Ksar El Kebir connections *)
  ("Ksar El Kebir", "Larache", 110); ("Ksar El Kebir", "Marrakech", 720);
  ("Ksar El Kebir", "Meknes", 340); ("Ksar El Kebir", "Midelt", 420);
  ("Ksar El Kebir", "Nador", 300); ("Ksar El Kebir", "Ouarzazate", 780);
  ("Ksar El Kebir", "Oujda", 680); ("Ksar El Kebir", "Rabat", 320);
  ("Ksar El Kebir", "Safi", 680); ("Ksar El Kebir", "Settat", 400);
  ("Ksar El Kebir", "Sidi Ifni", 980); ("Ksar El Kebir", "Tangier", 150);
  ("Ksar El Kebir", "Taroudant", 720); ("Ksar El Kebir", "Taza", 320);
  ("Ksar El Kebir", "Tetouan", 100); ("Ksar El Kebir", "Tiznit", 760);
  ("Ksar El Kebir", "Zagora", 900);

  (* Larache connections *)
  ("Larache", "Marrakech", 740); ("Larache", "Meknes", 360);
  ("Larache", "Midelt", 440); ("Larache", "Nador", 290);
  ("Larache", "Ouarzazate", 800); ("Larache", "Oujda", 660);
  ("Larache", "Rabat", 300); ("Larache", "Safi", 700);
  ("Larache", "Settat", 380); ("Larache", "Sidi Ifni", 1000);
  ("Larache", "Tangier", 130); ("Larache", "Taroudant", 740);
  ("Larache", "Taza", 340); ("Larache", "Tetouan", 80);
  ("Larache", "Tiznit", 780); ("Larache", "Zagora", 920);

  (* Marrakech connections *)
  ("Marrakech", "Meknes", 320); ("Marrakech", "Midelt", 400);
  ("Marrakech", "Nador", 860); ("Marrakech", "Ouarzazate", 200);
  ("Marrakech", "Oujda", 960); ("Marrakech", "Rabat", 240);
  ("Marrakech", "Safi", 200); ("Marrakech", "Settat", 280);
  ("Marrakech", "Sidi Ifni", 420); ("Marrakech", "Tangier", 660);
  ("Marrakech", "Taroudant", 170); ("Marrakech", "Taza", 680);
  ("Marrakech", "Tetouan", 700); ("Marrakech", "Tiznit", 260);
  ("Marrakech", "Zagora", 420);

  (* Meknes connections *)
  ("Meknes", "Midelt", 200); ("Meknes", "Nador", 460);
  ("Meknes", "Ouarzazate", 380); ("Meknes", "Oujda", 520);
  ("Meknes", "Rabat", 180); ("Meknes", "Safi", 480);
  ("Meknes", "Settat", 240); ("Meknes", "Sidi Ifni", 860);
  ("Meknes", "Tangier", 360); ("Meknes", "Taroudant", 580);
  ("Meknes", "Taza", 160); ("Meknes", "Tetouan", 340);
  ("Meknes", "Tiznit", 660); ("Meknes", "Zagora", 520);

  (* Midelt connections *)
  ("Midelt", "Nador", 500); ("Midelt", "Ouarzazate", 220);
  ("Midelt", "Oujda", 540); ("Midelt", "Rabat", 440);
  ("Midelt", "Safi", 580); ("Midelt", "Settat", 400);
  ("Midelt", "Sidi Ifni", 840); ("Midelt", "Tangier", 660);
  ("Midelt", "Taroudant", 400); ("Midelt", "Taza", 200);
  ("Midelt", "Tetouan", 640); ("Midelt", "Tiznit", 680);
  ("Midelt", "Zagora", 220);

  (* Nador connections *)
  ("Nador", "Ouarzazate", 760); ("Nador", "Oujda", 260);
  ("Nador", "Rabat", 500); ("Nador", "Safi", 780);
  ("Nador", "Settat", 520); ("Nador", "Sidi Ifni", 1100);
  ("Nador", "Tangier", 220); ("Nador", "Taroudant", 840);
  ("Nador", "Taza", 300); ("Nador", "Tetouan", 200);
  ("Nador", "Tiznit", 880); ("Nador", "Zagora", 920);

  (* Ouarzazate connections *)
  ("Ouarzazate", "Oujda", 600); ("Ouarzazate", "Rabat", 580);
  ("Ouarzazate", "Safi", 620); ("Ouarzazate", "Settat", 520);
  ("Ouarzazate", "Sidi Ifni", 820); ("Ouarzazate", "Tangier", 760);
  ("Ouarzazate", "Taroudant", 300); ("Ouarzazate", "Taza", 540);
  ("Ouarzazate", "Tetouan", 800); ("Ouarzazate", "Tiznit", 720);
  ("Ouarzazate", "Zagora", 180);

  (* Oujda connections *)
  ("Oujda", "Rabat", 660); ("Oujda", "Safi", 920);
  ("Oujda", "Settat", 660); ("Oujda", "Sidi Ifni", 1260);
  ("Oujda", "Tangier", 540); ("Oujda", "Taroudant", 960);
  ("Oujda", "Taza", 340); ("Oujda", "Tetouan", 480);
  ("Oujda", "Tiznit", 1040); ("Oujda", "Zagora", 780);

  (* Rabat connections *)
  ("Rabat", "Safi", 480); ("Rabat", "Settat", 70);
  ("Rabat", "Sidi Ifni", 820); ("Rabat", "Tangier", 240);
  ("Rabat", "Taroudant", 700); ("Rabat", "Taza", 460);
  ("Rabat", "Tetouan", 280); ("Rabat", "Tiznit", 740);
  ("Rabat", "Zagora", 740);

  (* Safi connections *)
  ("Safi", "Settat", 260); ("Safi", "Sidi Ifni", 620);
  ("Safi", "Tangier", 660); ("Safi", "Taroudant", 560);
  ("Safi", "Taza", 700); ("Safi", "Tetouan", 700);
  ("Safi", "Tiznit", 600); ("Safi", "Zagora", 680);

  (* Settat connections *)
  ("Settat", "Sidi Ifni", 660); ("Settat", "Tangier", 440);
  ("Settat", "Taroudant", 600); ("Settat", "Taza", 560);
  ("Settat", "Tetouan", 480); ("Settat", "Tiznit", 640);
  ("Settat", "Zagora", 680);

  (* Sidi Ifni connections *)
  ("Sidi Ifni", "Tangier", 900); ("Sidi Ifni", "Taroudant", 220);
  ("Sidi Ifni", "Taza", 880); ("Sidi Ifni", "Tetouan", 920);
  ("Sidi Ifni", "Tiznit", 160); ("Sidi Ifni", "Zagora", 400);

  (* Tangier connections *)
  ("Tangier", "Taroudant", 740); ("Tangier", "Taza", 400);
  ("Tangier", "Tetouan", 60); ("Tangier", "Tiznit", 780);
  ("Tangier", "Zagora", 920);

  (* Taroudant connections *)
  ("Taroudant", "Taza", 660); ("Taroudant", "Tetouan", 760);
  ("Taroudant", "Tiznit", 300); ("Taroudant", "Zagora", 480);

  (* Taza connections *)
  ("Taza", "Tetouan", 440); ("Taza", "Tiznit", 860);
  ("Taza", "Zagora", 580);

  (* Tetouan connections *)
  ("Tetouan", "Tiznit", 800); ("Tetouan", "Zagora", 900);

  (* Tiznit connections *)
  ("Tiznit", "Zagora", 480);

  ("Zagora", "Midelt", 480)
]


let () =
  Random.self_init (); 
  Printf.printf "--- Running Genetic Algorithm for TSP with 30 Moroccan Cities ---\n";
  let all_cities = thirty_moroccan_cities in
  let raw_distances = raw_thirty_moroccan_distances in 
  let start_city = "Casablanca" in

  let pop_size = 100 in        
  let num_generations = 500 in 
  let tournament_size = 7 in   
  let crossover_rate = 0.85 in  
  let mutation_rate = 0.05 in  
  let elitism_count = 3 in    

  if List.length raw_distances < 100 then 
     Printf.eprintf "Warning: raw_thirty_moroccan_distances seems very incomplete (found %d pairs, expected ~435). Results will be inaccurate.\n" (List.length raw_distances);

  match solve_tsp_ga all_cities raw_distances start_city
                      pop_size num_generations tournament_size
                      crossover_rate mutation_rate elitism_count with
  | Some (route, cost) ->
      Printf.printf "\n--- GA Solution Found ---\n";
      Printf.printf "Best Route: %s\n" (String.concat " -> " route);
      Printf.printf "Total Cost: %d km\n" cost
  | None -> Printf.printf "\n--- No valid solution found by GA or error occurred. ---\n"