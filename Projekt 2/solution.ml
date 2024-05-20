type bop = Mult | Div | Add | Sub | Eq | Lt | Gt | Leq | Geq | Neq
type ident = string

type expr =
  | Int of int
  | Bool of bool
  | Binop of bop * expr * expr
  | If of expr * expr * expr
  | Var of ident
  | Let of ident * expr * expr

let rec string_of_expr (expr : expr) : string =
  match expr with
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Var x -> x
  | Binop (op, e1, e2) ->
      let e1' = string_of_expr e1 in
      let e2' = string_of_expr e2 in
      let op' =
        match op with
        | Add -> "+"
        | Sub -> "-"
        | Mult -> "*"
        | Div -> "/"
        | Eq -> "="
        | Neq -> "<>"
        | Gt -> ">"
        | Lt -> "<"
        | Geq -> ">="
        | Leq -> "<="
      in
      e1' ^ " " ^ op' ^ " " ^ e2'
  | If (p, t, e) -> "if " ^ string_of_expr p ^ " then " ^ string_of_expr t ^ " else " ^ string_of_expr e
  | Let (v, s, e) -> "let " ^ v ^ " = " ^ string_of_expr s ^ " in " ^ string_of_expr e

module M = Set.Make (String)

type candidate = Expr_set of M.t | The_chosen_one of string

let rec find_candidate exp bound_vars : candidate =
  match exp with
  | Int _ | Bool _ | Var _ -> Expr_set M.empty
  | Binop (op, e1, e2) -> (
      let s1 = find_candidate e1 bound_vars in
      let s2 = find_candidate e2 bound_vars in
      match (s1, s2) with
      | _, The_chosen_one tco | The_chosen_one tco, _ -> The_chosen_one tco
      | Expr_set es1, Expr_set es2 -> (
          let inter = M.inter es1 es2 in
          try The_chosen_one (M.min_elt inter)
          with Not_found -> Expr_set (M.add (string_of_expr exp ^ bound_vars) (M.union es1 es2))))
  | If (p, t, e) -> (
      let s1 = find_candidate p bound_vars in
      let s2 = find_candidate t bound_vars in
      let s3 = find_candidate e bound_vars in
      match (s1, s2, s3) with
      | _, The_chosen_one tco, _
      | _, _, The_chosen_one tco
      | The_chosen_one tco, _, _ -> The_chosen_one tco
      | Expr_set es1, Expr_set es2, Expr_set es3 -> (
          let inter = M.union (M.union (M.inter es1 es2) (M.inter es2 es3)) (M.inter es1 es3) in
          try The_chosen_one (M.min_elt inter)
          with Not_found -> Expr_set (M.add (string_of_expr exp ^ bound_vars) (M.union (M.union es1 es2) es3))))
  | Let (v, s, e) -> (
      let s1 = find_candidate s bound_vars in
      let s2 = find_candidate e (bound_vars ^ v) in
      match (s1, s2) with
      | _, The_chosen_one tco | The_chosen_one tco, _ -> The_chosen_one tco
      | Expr_set es1, Expr_set es2 -> (
          let inter = M.inter es1 es2 in
          try The_chosen_one (M.min_elt inter)
          with Not_found -> Expr_set (M.add (string_of_expr exp ^ bound_vars) (M.union es1 es2))))

let rec find_expr (exp : expr) (bound_vars : string) (i : string) : expr * string =
  match exp with
  | Binop (op, e1, e2) -> (
      if string_of_expr exp ^ bound_vars = i then (exp, bound_vars)
      else
        try find_expr e1 bound_vars i
        with Not_found -> find_expr e2 bound_vars i)
  | If (p, t, e) -> (
      if string_of_expr exp ^ bound_vars = i then (exp, bound_vars)
      else
        try find_expr p bound_vars i
        with Not_found -> (
          try find_expr t bound_vars i
          with Not_found -> find_expr e bound_vars i))
  | Let (v, s, e) -> (
      if string_of_expr exp ^ bound_vars = i then (exp, bound_vars)
      else
        try find_expr s bound_vars i
        with Not_found -> find_expr e (bound_vars ^ v) i)
  | _ -> raise Not_found

let rec subst (exp : expr) (cur_bound : string) (bound : string) (s : expr) (w : expr) : expr =
  match exp with
  | Binop (op, e1, e2) ->
      if exp = s && cur_bound = bound then w
      else Binop (op, subst e1 cur_bound bound s w, subst e2 cur_bound bound s w)
  | If (p, t, e) ->
      if exp = s && cur_bound = bound then w
      else If (p, subst t cur_bound bound s w, subst e cur_bound bound s w)
  | Let (v, s', e) ->
      if exp = s && cur_bound = bound then w
      else Let(v, subst s' (cur_bound ^ v) bound s w, subst e (cur_bound ^ v) bound s w)
  | other -> other

let cse (exp : expr) : expr option =
  match find_candidate exp "" with
  | Expr_set _ -> None
  | The_chosen_one tco ->
      let s, bound = find_expr exp "" tco in
      let v = "#(" ^ tco ^ ")" in
      Some (Let (v, s, subst exp "" bound s (Var v)))
