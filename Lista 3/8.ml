type 'a tree =
  | Leaf
  | Node of 'a tree * 'a * 'a tree

let t =
  Node (Node (Leaf, 2, Leaf),
    5,
    (Node (Node (Leaf, 6, Leaf),
      8,
      Node (Leaf, 9, Leaf))))

let rec insert_bst v t =
  match t with
  | Leaf -> Node (Leaf, v, Leaf)
  | Node (l, x, r) ->
    if v < x then
      Node (insert_bst v l, x, r)
    else
      Node (l, x, insert_bst v r)

let rec fold_tree f a t =
  match t with
  | Leaf -> a
  | Node (l, v, r) -> f (fold_tree f a l) v (fold_tree f a r)

let rec create_tree xs =
  match xs with
  | [] -> Leaf
  | head :: tail -> insert_bst head (create_tree tail)

let inorder t = fold_tree (fun a b c -> a @ [b] @ c) [] t

let sort_list xs =
  let t = create_tree xs in
  inorder t