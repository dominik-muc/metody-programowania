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
    else if v > x
      Node (l, x, insert_bst v r)
    else t