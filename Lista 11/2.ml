let find ( type t ) p ( xs : t list ) : t =
  let exception Found of t in
  try List.iter ( fun x -> if p x then raise (Found x) ) xs; raise Not_found
  (* To samo co: try List.fold_left ( fun _ x -> if p x then raise (Found x) ) () xs; raise Not_found *)
  with Found x -> x