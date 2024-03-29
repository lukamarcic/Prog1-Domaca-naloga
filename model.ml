(* Pomožni tip, ki predstavlja mrežo *)

type 'a grid = 'a Array.t Array.t

(* Funkcije za prikaz mreže.
   Te definiramo najprej, da si lahko z njimi pomagamo pri iskanju napak. *)

(* Razbije seznam [lst] v seznam seznamov dolžine [size] *)
let chunkify size lst =
  let rec aux chunk chunks n lst =
    match (n, lst) with
    | _, [] when chunk = [] -> List.rev chunks
    | _, [] -> List.rev (List.rev chunk :: chunks)
    | 0, _ :: _ -> aux [] (List.rev chunk :: chunks) size lst
    | _, x :: xs -> aux (x :: chunk) chunks (n - 1) xs
  in
  aux [] [] size lst

let string_of_list string_of_element sep lst =
  lst |> List.map string_of_element |> String.concat sep

let string_of_nested_list string_of_element inner_sep outer_sep =
  string_of_list (string_of_list string_of_element inner_sep) outer_sep

let string_of_row string_of_cell row =
  let string_of_cells =
    row |> Array.to_list |> chunkify 3
    |> string_of_nested_list string_of_cell "" "│"
  in
  "┃" ^ string_of_cells ^ "┃\n"

let print_grid string_of_cell grid =
  let ln = "───" in
  let big = "━━━" in
  let divider = "┠" ^ ln ^ "┼" ^ ln ^ "┼" ^ ln ^ "┨\n" in
  let row_blocks =
    grid |> Array.to_list |> chunkify 3
    |> string_of_nested_list (string_of_row string_of_cell) "" divider
  in
  Printf.printf "┏%s┯%s┯%s┓\n" big big big;
  Printf.printf "%s" row_blocks;
  Printf.printf "┗%s┷%s┷%s┛\n" big big big

(* Funkcije za dostopanje do elementov mreže *)

let get_row (grid : 'a grid) (row_ind : int) = grid.(row_ind)

let rows grid = List.init 9 (get_row grid)

let get_column (grid : 'a grid) (col_ind : int) =
  Array.init 9 (fun row_ind -> grid.(row_ind).(col_ind))

let columns grid = List.init 9 (get_column grid)

let get_box (grid : 'a grid) (box_ind : int) =
  let m = match box_ind with
    | 0 -> 0
    | 1 -> 0
    | 2 -> 0
    | 3 -> 3
    | 4 -> 3
    | 5 -> 3
    | _ -> 6
  in
  let n = (box_ind mod 3) * 3
  in
  let box = Array.init 3
  (
    fun x -> Array.init 3 (fun y -> grid.(m + x).(n + y))
  )
  in
  box

let boxes grid = List.init 9 (get_box grid)

(* Funkcije za ustvarjanje novih mrež *)

let map_grid (f : 'a -> 'b) (grid : 'a grid) : 'b grid = 
  let mapped = Array.init 9 (fun i -> Array.map f (grid.(i))) in
  Array.init 9 (fun j -> mapped.(j))

let copy_grid (grid : 'a grid) : 'a grid = map_grid (fun x -> x) grid

let foldi_grid (f : int -> int -> 'a -> 'acc -> 'acc) (grid : 'a grid)
    (acc : 'acc) : 'acc =
  let acc, _ =
    Array.fold_left
      (fun (acc, row_ind) row ->
        let acc, _ =
          Array.fold_left
            (fun (acc, col_ind) cell ->
              (f row_ind col_ind cell acc, col_ind + 1))
            (acc, 0) row
        in
        (acc, row_ind + 1))
      (acc, 0) grid
  in
  acc

let row_of_string cell_of_char str =
  List.init (String.length str) (String.get str) |> List.filter_map cell_of_char

let grid_of_string cell_of_char str =
  let grid =
    str |> String.split_on_char '\n'
    |> List.map (row_of_string cell_of_char)
    |> List.filter (function [] -> false | _ -> true)
    |> List.map Array.of_list |> Array.of_list
  in
  if Array.length grid <> 9 then failwith "Nepravilno število vrstic";
  if Array.exists (fun x -> x <> 9) (Array.map Array.length grid) then
    failwith "Nepravilno število stolpcev";
  grid

(* Model za vhodne probleme *)

type problem = { initial_grid : int option grid }

let print_problem problem : unit =
  let f = function
  | None -> " "
  | Some n -> string_of_int n
  in
  print_grid f problem.initial_grid

let problem_of_string str =
  let cell_of_char = function
    | ' ' -> Some None
    | c when '1' <= c && c <= '9' -> Some (Some (Char.code c - Char.code '0'))
    | _ -> None
  in
  { initial_grid = grid_of_string cell_of_char str }

(* Model za izhodne rešitve *)

type solution = int grid

let print_solution solution = print_grid string_of_int solution

let box_to_row box =
  let sez_vrstic = Array.to_list box in
  Array.concat (sez_vrstic)

let pravilni_row = [|1; 2; 3; 4; 5; 6; 7; 8; 9|]

let sortiraj row =
  let row2 = Array.copy row in
  Array.sort Stdlib.compare row2;
  row2

let preveri_row row =
  let sortiran = sortiraj row in
  sortiran = pravilni_row

let preveri_grid grid =
  let bool_rows = Array.for_all preveri_row (Array.of_list (rows grid)) in
  let bool_columns = Array.for_all preveri_row (Array.of_list (columns grid)) in
  let box_list = Array.map box_to_row (Array.of_list (boxes grid)) in
  let bool_boxes = Array.for_all preveri_row box_list in
  bool_rows && bool_columns && bool_boxes

let int_op_to_int  = function
  | None -> 0
  | Some n -> n

let problem_and_solution problem solution =
  let elementi_solution = Array.to_list (Array.concat (rows solution)) in
  let elementi_problem_temp = Array.to_list (Array.concat (rows problem.initial_grid)) in
  let elementi_problem = List.map int_op_to_int elementi_problem_temp in
  let rec po_elementih lst1 lst2 =
    match lst1 with
    | [] -> true
    | x :: xs ->
      match lst2 with
      | [] -> false
      | y :: ys -> if x = y || x = 0 then po_elementih xs ys else false
  in
  po_elementih elementi_problem elementi_solution
  

let is_valid_solution problem solution = preveri_grid solution && problem_and_solution problem solution