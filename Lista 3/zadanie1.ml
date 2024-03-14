let rec insert n xs =
  match xs with
  | [] -> [n]
  | x :: xs' ->
    if n < x
    then n :: xs
    else x :: insert n xs'

let rec insertion_sort xs =
  match xs with
  | [] -> []
  | x :: xs' -> insert x (insertion_sort xs')

  let rec insert_desc n xs =
    match xs with
    | [] -> [n]
    | x :: xs' ->
      if n > x
      then n :: xs
      else x :: insert_desc n xs'

let rec insertion_sort_desc xs =
  match xs with
  | [] -> []
  | x :: xs' -> insert_desc x (insertion_sort_desc xs')

let rec insert_gen lt n xs =
  match xs with
  | [] -> [n]
  | x :: xs' ->
    if lt n x
    then n :: xs
    else x :: insert_gen lt n xs'

let rec insertion_sort_gen lt xs =
  match xs with
  | [] -> []
  | x :: xs' -> insert_gen lt x (insertion_sort_gen lt xs')

let pair_lex (a1, a2) (b1, b2) =
  if a1 < b1 then true
  else if b1 < a1 then false
  else a2 < b2

let pair_lex (a1, a2) (b1, b2) =
  if a1 < b1 then true
  else if b1 < a1 then false
  else a2 < b2

let pair_lex_generic lt1 lt2 (a1, a2) (b1, b2) =
  if lt1 a1 b1 then true
  else if lt1 b1 a1 then false
  else lt2 a2 b2

let add1 x = x + 1

let rec add1_to_all xs =
  match xs with
  | [] -> []
  | x :: xs' -> add1 x :: add1_to_all xs'

let rec add1_to_all' = function
  | [] -> []
  | x :: xs' -> add1 x :: add1_to_all' xs'

let rec string_list_of_int_list = function
| [] -> []
| x :: xs' -> string_of_int x :: string_list_of_int_list xs'

let rec map f = function
  | [] -> []
  | x :: xs' -> f x :: map f xs'

let rec only_positive = function
| [] -> []
| x :: xs -> if x > 0 then x :: only_positive xs else only_positive xs

let rec only_nonempty = function
| [] -> []
| x :: xs -> if x <> [] then x :: only_nonempty xs else only_nonempty xs

let rec filter p = function
| [] -> []
| x :: xs -> if p x then x :: filter p xs else filter p xs

let rec append xs ys =
  match xs with
  | [] -> ys
  | x :: xs' -> x :: append xs' ys

let rec fold_right f xs a =
  match xs with
  | [] -> a
  | x :: xs' -> f x (fold_right f xs' a)

let new_append xs ys = fold_right (fun x a -> x :: a) xs ys
let new_append' xs ys = fold_right List.cons xs ys

let new_map f xs = fold_right (fun y ys -> f y :: ys) xs []

let new_filter p xs = fold_right (fun y ys -> if p y then y :: ys else ys) xs []

let length xs =
  let rec it xs acc =
    match xs with
    | [] -> acc
    | x :: xs' -> it xs' (acc + 1)
  in it xs 0

let sum xs =
  let rec it xs acc =
    match xs with
    | [] -> acc
    | x :: xs' -> it xs' (acc + x)
  in it xs 0

let rev xs =
  let rec it xs acc =
    match xs with
    | [] -> acc
    | x :: xs' -> it xs' (x :: acc)
  in it xs []

let fold_left f a xs =
  let rec it xs acc =
    match xs with
    | [] -> acc
    | x :: xs' -> it xs' (f acc x)
  in it xs a

let new_length xs = fold_left (fun acc y -> acc + 1) 0 xs

let new_sum xs = fold_left (+) 0 xs

let new_rev xs = fold_left (fun acc y -> y :: acc) [] xs

let new_sum_worse xs = fold_right (+) xs 0

let new_map_reversing f xs = fold_left (fun ys y -> f y :: ys) [] xs

let new_map' f xs = rev (new_map_reversing f xs)

let new_map'' f xs = new_map_reversing f xs |> rev

(* fold_right f [x1; x2; x3] a ->
   f x1 (fold_right f [x2; x3] a)
   f x1 (f x2 (fold_right f [x3] a))
    f x1 (f x2 (f x3 (fold_right f [] a)))
    f x1 (f x2 (f x3 a))

    fold_right (#) [x1; x2; x3] a ->
    x1 # (x2 # (x3 # a))

    fold_left f a [x1; x2; x3] ->
    it [x1; x2; x3] a ->
    it [x2; x3] (f a x1) ->
    it [x3] (f (f a x1) x2) ->
    it [] (f (f (f a x1) x2) x3) ->
    (f (f (f a x1) x2) x3)

    fold_left (#) a [x1; x2; x3] ->
    (((a # x1) # x2) # x3)

   *)

type comp = Lt | Eq | Gt

let compare x y =
  if x < y then Lt
  else if x = y then Eq
  else Gt

type int_or_string = Int of int | String of string

let string_of_int_or_string ios =
  match ios with
  | Int i -> string_of_int i
  | String s -> s

type int_tree = LeafI | NodeI of int_tree * int * int_tree

let rec sum_tree t =
  match t with
  | LeafI -> 0
  | NodeI (l, v, r) -> sum_tree l + v + sum_tree r

type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

type ('a, 'b) tree2 = Leaf2 | Node2 of ('a, 'b) tree2 * 'a * 'b * ('a, 'b) tree2

let rec map_tree f t =
  match t with
  | Leaf -> Leaf
  | Node (l, v, r) -> Node (map_tree f l, f v, map_tree f r)

let rec filter_tree p t =
  match t with
  | Leaf -> Leaf
  | Node (l, v, r) ->
      if p v 
      then Node (filter_tree p l, v, filter_tree p r)
      else Leaf

let rec fold_tree f a t =
  match t with
  | Leaf -> a
  | Node (l, v, r) -> f (fold_tree f a l) v (fold_tree f a r)

let new_map_tree f t = fold_tree (fun l v r -> Node (l, f v, r)) Leaf t

let sum_tree t = fold_tree (fun l v r -> v + l + r) 0 t

let contains p t = fold_tree (fun l v r -> p v || l || r) false t

let flatten t = fold_tree (fun l v r -> l @ [v] @ r) [] t

let example_tree = Node (Node (Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf))

let rec find_bst x t =
  match t with
  | Leaf -> false
  | Node (l, v, r) -> 
      if x = v then true
      else if x < v then find_bst x l
      else find_bst x r