type 'a tree =
  | Leaf
  | Node of 'a tree * 'a * 'a tree

let t =
  Node (Node (Leaf, 2, Leaf),
    5,
    (Node (Node (Leaf, 6, Leaf),
      8,
      Node (Leaf, 9, Leaf))))

let rec fold_tree f a t =
  match t with
  | Leaf -> a
  | Node (l, v, r) -> f (fold_tree f a l) v (fold_tree f a r)

let tree_product t = fold_tree (fun a b c -> a*b*c) 1 t

let tree_flip t = fold_tree (fun a b c -> Node(c, b, a)) Leaf t

let tree_height t = fold_tree (fun a b c -> max a c + 1) 0 t

let tree_span t =
  let merge_spans (min1, max1) v (min2, max2) =
    (min (min min1 v) min2, max (max max1 v) max2)
  in
  fold_tree merge_spans (max_int, min_int) t

let preorder t = fold_tree (fun a b c -> b :: a @ c) [] t