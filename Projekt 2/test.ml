(* Binary operation types *)
type bop = Mult | Div | Add | Sub | Eq | Lt | Gt | Leq | Geq | Neq

(* Identifier type *)
type ident = string

(* Expression types *)
type expr =
  | Int of int
  | Bool of bool
  | Binop of bop * expr * expr
  | If of expr * expr * expr
  | Var of ident
  | Let of ident * expr * expr

let rec string_of_expr (expr : expr) : string = 
  match expr with
  | Int n -> (string_of_int n)
  | Bool b -> (string_of_bool b)
  | Var x -> x
  | Binop(op, e1, e2) ->
    let e1' = string_of_expr e1 in
    let e2' = string_of_expr e2 in
    let op' = match op with
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
    in e1' ^ " " ^ op' ^ " " ^ e2'
  | If(p, t, e) -> "if " ^ (string_of_expr p) ^ " then " ^ (string_of_expr t) ^ " else " ^ (string_of_expr e)
  | Let(v, s, e) -> "let " ^ v ^ " = " ^ (string_of_expr s) ^ " in " ^ (string_of_expr e)

(* Import Hashtbl module *)

module M = Set.Make(String)

type candidate = Expr_set of M.t | The_chosen_one of string

let rec find_set exp bound_vars : candidate =
  match exp with
  | Int _ | Bool _ | Var _ -> Expr_set M.empty
  | Binop(op, e1, e2) -> 
    begin
      let s1 = find_set e1 bound_vars in
      let s2 = find_set e2 bound_vars in
      match s1, s2 with
      | (_, The_chosen_one tco)
      | (The_chosen_one tco, _) -> The_chosen_one tco
      | Expr_set es1, Expr_set es2 ->
        let inter = M.inter es1 es2 in
        try The_chosen_one (M.min_elt inter)
        with Not_found -> (Expr_set (M.add ((string_of_expr exp) ^ bound_vars) (M.union es1 es2)))
    end
  | If(p, t, e) -> 
    begin
      let s1 = find_set p bound_vars in
      let s2 = find_set t bound_vars in
      let s3 = find_set e bound_vars in
      match s1, s2, s3 with
      | (_, The_chosen_one tco, _)
      | (_, _, The_chosen_one tco)
      | (The_chosen_one tco, _, _) -> The_chosen_one tco
      | Expr_set es1, Expr_set es2, Expr_set es3 ->
        let inter = M.union (M.union (M.inter es1 es2) (M.inter es2 es3)) (M.inter es1 es3) in
        try The_chosen_one (M.min_elt inter)
        with Not_found -> (Expr_set (M.add ((string_of_expr exp) ^ bound_vars) (M.union (M.union es1 es2) es3)))
    end
  | Let(v, s, e) ->
    begin
      let s1 = find_set s bound_vars in
      let s2 = find_set e (bound_vars ^ v) in
      match s1, s2 with
      | (_, The_chosen_one tco)
      | (The_chosen_one tco, _) -> The_chosen_one tco
      | Expr_set es1, Expr_set es2 ->
        let inter = M.inter es1 es2 in
        try The_chosen_one (M.min_elt inter)
        with Not_found -> (Expr_set (M.add ((string_of_expr exp) ^ bound_vars) (M.union es1 es2)))
    end

let rec retrieve (exp : expr) (bound_vars : string) (i : string) : expr option =
  match exp with
  | Binop(op, e1, e2) ->
    begin
      if ((string_of_expr exp) ^ bound_vars) = i then Some(exp) 
      else 
        try retrieve e1 bound_vars i
        with Not_found -> retrieve e2 bound_vars i
    end
  | _ -> raise Not_found


let rec subst (exp : expr) (s : expr) (w : expr) : expr = 
  match exp with
  | Binop(op, e1, e2) ->
    begin
      if exp = s then w
      else Binop(op, subst e1 s w, subst e2 s w)
    end
  | _ -> failwith "TOTAL FAILURE"

let cse (exp : expr) : expr option = 
  match find_set exp "" with
  | Expr_set _ -> None
  | The_chosen_one tco -> 
    let s = match retrieve exp "" tco with Some e -> e | None -> failwith "TOTAL FAILURE" in
    Some (Let("#", s, subst exp s (Var "#")))

(**)
