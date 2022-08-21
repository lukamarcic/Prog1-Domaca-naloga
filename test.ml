type 'a grid = 'a Array.t Array.t

let var = [|
  [|1; 2; 3; 4; 5; 6; 7; 8; 9|];
  [|4; 5; 6; 7; 8; 9; 1; 2; 3|];
  [|7; 8; 9; 1; 2; 3; 4; 5; 6|];
  [|2; 3; 1; 5; 6; 4; 8; 9; 7|];
  [|5; 6; 4; 8; 9; 7; 2; 3; 1|];
  [|8; 9; 7; 2; 3; 1; 5; 6; 4|];
  [|3; 1; 2; 6; 4; 5; 9; 7; 8|];
  [|6; 4; 5; 9; 7; 8; 3; 1; 2|];
  [|9; 7; 8; 3; 1; 2; 6; 4; 5|]
|]

let var2 = [|
  [|None; Some 3; Some 3; Some 4; Some 5; Some 6; Some 7; Some 8; Some 9|];
  [|Some 4; None; Some 6; Some 7; Some 8; Some 9; Some 1; Some 2; Some 3|];
  [|Some 7; Some 8; Some 9; Some 1; Some 2; Some 3; Some 4; Some 5; Some 6|];
  [|Some 2; Some 3; Some 1; Some 5; Some 6; Some 4; Some 8; Some 9; Some 7|];
  [|Some 5; Some 6; Some 4; Some 8; Some 9; Some 7; Some 2; Some 3; Some 1|];
  [|Some 8; Some 9; Some 7; Some 2; Some 3; Some 1; Some 5; Some 6; Some 4|];
  [|Some 3; Some 1; Some 2; Some 6; Some 4; Some 5; Some 9; Some 7; Some 8|];
  [|Some 6; Some 4; Some 5; Some 9; Some 7; Some 8; Some 3; Some 1; Some 2|];
  [|Some 9; Some 7; Some 8; Some 3; Some 1; Some 2; Some 6; Some 4; Some 5|]
|]


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




let box_to_row box =
  let sez_vrstic = Array.to_list box in
  Array.concat (sez_vrstic)

let pravilni_row = [|1; 2; 3; 4; 5; 6; 7; 8; 9|]

let int_op_to_int  = function
  | None -> 0
  | Some n -> n

let problem_and_solution problem solution =
  let elementi_solution = Array.to_list (Array.concat (rows solution)) in
  let elementi_problem_temp = Array.to_list (Array.concat (rows problem)) in
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
  





