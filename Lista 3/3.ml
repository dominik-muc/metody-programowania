(* Function to construct an n-element list by applying f to each element from 0 to n - 1 *)
let build_list n f =
  let rec helper i acc =
    if i < 0 then acc
    else helper (i - 1) (f i :: acc)
  in
  helper (n - 1) []

let negatives n = build_list n (fun a -> -a - 1)

let reciprocals n = build_list n (fun a -> string_of_int(1) ^ "/" ^ string_of_int(a+1))

let evens n = build_list n (fun a -> 2*a)

let identitiyM n = build_list n (fun a -> build_list n (fun x -> if x = a then 1 else 0))