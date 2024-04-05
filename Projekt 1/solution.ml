let ( let* ) xs ys = List.concat_map ys xs

let rec choose m n =
  if m > n then [] else m :: choose (m+1) n


let rec insert_n_variables n v ls =
  if n <= 0 then ls else insert_n_variables (n - 1) v (v :: ls)



let rec builder available sum ps acc = 
  match ps with
  | p :: ps ->
    let* a = choose 0 (available - sum) in
    if ps = [] then
      [acc] @ builder (available - a - p) (sum - p) ps (insert_n_variables p true (insert_n_variables a false acc))
    else
      [acc] @ builder (available - a - p - 1) (sum - p) ps (false :: (insert_n_variables p true (insert_n_variables a false acc)))
  | _ -> [insert_n_variables (available) false acc]

let build_row (ps : int list) (n : int) : bool list list = 
  let* a = builder n (List.fold_left (+) 0 ps) ps [] in
  [List.rev a]
  

let build_candidate pss n = 
  let rec helper pss n acc =
    match pss with
    | ps :: pss ->
      let rows = List.filter (fun a -> List.length a = n) (build_row ps n) in
      List.concat_map (fun a -> helper pss n (acc @ [a])) rows
    | _ -> [acc]
  in helper pss n []

let parse_row (xs : bool list) : int list =
  let rec helper xs curr acc : int list =
    match xs with
    | [] -> if curr > 0 then curr :: acc else acc
    | x :: xs ->
      if x then 
        helper xs (curr + 1) acc
      else
        helper xs 0 (if curr = 0 then acc else (curr :: acc))
  in helper xs 0 []

let verify_row (ps : int list) (xs : bool list) = (List.rev ps = parse_row xs)

let rec verify_rows (pss : int list list) (xss : bool list list) = 
  match (pss, xss) with
  | (ps :: pss, xs :: xss) -> if verify_row ps xs then verify_rows pss xss else false
  | _ -> true


let row1 = [1;2;3]
let row2 = [4;5;6]
let row3 = [7;8;9]

let rec append_vertical xs ys =
  match (xs, ys) with
    | ([], ys) -> ys
    | (xs, []) -> (fun a -> let* x = a in [[x]]) xs
    | (x :: xs, y :: ys) -> (x :: y) :: append_vertical xs ys

let rec transpose xss =
  match xss with
    | [] -> []
    | xs :: xss -> append_vertical (xs) (transpose xss)

type nonogram_spec = {rows: int list list; cols: int list list}

let solve_nonogram nono =
  build_candidate (nono.rows) (List.length (nono.cols))
  |> List.filter (fun xss -> transpose xss |> verify_rows nono.cols) 
  
let example_1 = {
  rows = [[2];[1];[1]];
  cols = [[1;1];[2]]
}

let example_2 = {
  rows = [[2];[2;1];[1;1];[2]];
  cols = [[2];[2;1];[1;1];[2]]
}

let big_example = {
  rows = [[1;2];[2];[1];[1];[2];[2;4];[2;6];[8];[1;1];[2;2]];
  cols = [[2];[3];[1];[2;1];[5];[4];[1;4;1];[1;5];[2;2];[2;1]]
}