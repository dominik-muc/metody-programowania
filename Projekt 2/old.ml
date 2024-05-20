(* let fresh_var =
  let counter = ref 0 in
  fun () ->
    incr counter;
    "v" ^ string_of_int !counter

(* Helper function to manage sets of identifiers *)
module IdentSet = Set.Make(String)

(* Check if an expression is trivial (single literal or variable) *)
let is_trivial expr =
  match expr with
  | Int _ | Bool _ | Var _ -> true
  | _ -> false

(* Function to check if an expression is free of external bindings *)
let rec is_free_of_bindings bound_vars expr =
  match expr with
  | Int _ | Bool _ -> true
  | Var v -> IdentSet.mem v bound_vars
  | Binop (_, e1, e2) ->
    is_free_of_bindings bound_vars e1 && is_free_of_bindings bound_vars e2
  | If (e1, e2, e3) ->
    is_free_of_bindings bound_vars e1 &&
    is_free_of_bindings bound_vars e2 &&
    is_free_of_bindings bound_vars e3
  | Let (v, e1, e2) ->
    let new_bound_vars = IdentSet.add v bound_vars in
    is_free_of_bindings bound_vars e1 && is_free_of_bindings new_bound_vars e2

(* CSE Transformation Function *)
let rec cse expr =
  match expr with
  | Int _ | Bool _ | Var _ -> None
  | Binop (op, e1, e2) ->
    begin match (cse e1, cse e2) with
    | (Some new_e1, _) -> Some (Binop (op, new_e1, e2))
    | (_, Some new_e2) -> Some (Binop (op, e1, new_e2))
    | (None, None) -> try_replace e1 e2 op
    end
  | If (e1, e2, e3) ->
    begin match (cse e1, cse e2, cse e3) with
    | (Some new_e1, _, _) -> Some (If (new_e1, e2, e3))
    | (_, Some new_e2, _) -> Some (If (e1, new_e2, e3))
    | (_, _, Some new_e3) -> Some (If (e1, e2, new_e3))
    | _ -> None
    end
  | Let (v, e1, e2) ->
    begin match (cse e1, cse e2) with
    | (Some new_e1, _) -> Some (Let (v, new_e1, e2))
    | (_, Some new_e2) -> Some (Let (v, e1, new_e2))
    | _ -> None
    end

(* Improved try_replace function to avoid trivial replacements *)
and try_replace e1 e2 op =
  if is_free_of_bindings IdentSet.empty e1 && e1 = e2 && not (is_trivial e1) then
    let v = fresh_var () in
    Some (Let (v, e1, Binop (op, Var v, Var v)))
  else
    None *)