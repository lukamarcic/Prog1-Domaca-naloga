type available = { loc : int * int; possible : int list }

type state = { 
  problem : Model.problem;
  current_grid : int option Model.grid;
  current_koordinate : int * int;
  current_sez: int option list
  }

let print_state (state : state) : unit =
  Model.print_grid
    (function None -> " " | Some digit -> string_of_int digit)
    state.current_grid

type response = Solved of Model.solution | Unsolved of state | Fail of state

(* Najprej funkcije ki preverijo, če se elementi ne ponavljajo v row, column in box*)
let preveri_da_ni_napake array = 
  let seznam_elementov = [|0; 0; 0; 0; 0; 0; 0; 0; 0|] in
  let increase_counter array i =
    let x = array.(i) in
    Array.set array i (x + 1)
  in
  let rec preveri_aux array = function
  | 9 -> ()
  | i -> 
    let trenutni_el = array.(i) in
    match trenutni_el with
    | None -> preveri_aux array (i + 1)
    | Some n ->
      increase_counter seznam_elementov (n-1);
      preveri_aux array (i + 1)
  in
  preveri_aux array 0;
  Array.for_all (fun x -> x < 2) seznam_elementov
  
let preveri_row grid i =
  let row = Model.get_row grid i in
  preveri_da_ni_napake row

let preveri_column grid i =
  let column = Model.get_column grid i in
  preveri_da_ni_napake column

let preveri_box grid i =
  let box = Model.get_box grid i in
  let box_row = Model.box_to_row box in
  preveri_da_ni_napake box_row

(* Funkcije, ki najdejo prazno polje in izpišejo potencialne vrednosti*)
let rec prvi_ustrezen grid =
  let rec f_aux grid_aux i j =
    let trenutni = grid_aux.(i).(j) in
    match trenutni with
    | None -> (i, j)
    | Some n ->
      match j with
      | 8 -> f_aux grid_aux (i + 1) 0
      | _ -> f_aux grid_aux i (j + 1)
  in
  f_aux grid 0 0
  
let ustrezen_ugib grid i j =
  let row = preveri_row grid i in
  let column = preveri_column grid j in
  let box = preveri_box grid (i - (i mod 3) + (j / 3)) in
  row && column && box

let potencialni_ugibi grid i j =
  let novi_grid = Model.copy_grid grid in
  let potencialni_ugibi = ref [] in
  for x = 1 to 9 do
    Array.set novi_grid.(i) j (Some x);
    if ustrezen_ugib novi_grid i j then potencialni_ugibi := Some x :: !potencialni_ugibi
  done;
  !potencialni_ugibi

(* Funkcije, ki začnejo iskati rešitve*)
let initialize_state (problem : Model.problem) : state = 
  {
    problem = problem;
    current_grid = Model.copy_grid problem.initial_grid;
    current_koordinate = (0, 0);
    current_sez = []
    }

let validate_state (state : state) : response =
  let unsolved =
    Array.exists (Array.exists Option.is_none) state.current_grid
  in
  if unsolved then Unsolved state
  else
    (* Option.get ne bo sprožil izjeme, ker so vse vrednosti v mreži oblike Some x *)
    let solution = Model.map_grid Option.get state.current_grid in
    if Model.is_valid_solution state.problem solution then Solved solution
    else Fail state

let branch_state (state : state) : (state * state) option =
  if state.current_sez = [Some 0] then None else
    let i, j = state.current_koordinate in
    let grid_trenutni = Model.copy_grid state.current_grid in
    let sez2 =
    match state.current_sez with
      | [] -> [Some 0]
      | x :: xs ->
        Array.set grid_trenutni.(i) j (x);
        xs
    in
    let state2 = {state with
      current_grid = grid_trenutni;
      current_sez = sez2
      }
    in
    Some (state, state2)
  
(* pogledamo, če trenutno stanje vodi do rešitve *)
let rec solve_state (state : state) =
  (* uveljavimo trenutne omejitve in pogledamo, kam smo prišli *)
  let (i, j) = prvi_ustrezen state.current_grid in
  let mozni = potencialni_ugibi state.current_grid i j in
  match mozni with
  | [] -> None
  | x :: xs ->
    Array.set state.current_grid.(i) j (x);
    let state2 = {state with
      current_sez = xs;
      current_koordinate = (i, j)
      }
  in
  match validate_state state2 with
  | Solved solution ->
      (* če smo našli rešitev, končamo *)
      Some solution
  | Fail fail ->
      (* prav tako končamo, če smo odkrili, da rešitev ni *)
      None
  | Unsolved state2 ->
      (* če še nismo končali, raziščemo stanje, v katerem smo končali *)
      explore_state state2

and explore_state (state : state) =
  (* pri raziskovanju najprej pogledamo, ali lahko trenutno stanje razvejimo *)
  match branch_state state with
  | None ->
      (* če stanja ne moremo razvejiti, ga ne moremo raziskati *)
      None
  | Some (st1, st2) -> (
      (* če stanje lahko razvejimo na dve možnosti, poizkusimo prvo *)
      match solve_state st1 with
      | Some solution ->
          (* če prva možnost vodi do rešitve, do nje vodi tudi prvotno stanje *)
          Some solution
      | None ->
          (* če prva možnost ne vodi do rešitve, raziščemo še drugo možnost *)
          explore_state st2 )

let solve_problem (problem : Model.problem) =
  problem |> initialize_state |> solve_state
