(* Abstract Syntax Tree (AST) definition *)
type bop = Mult | Div | Add | Sub | Eq | Lt | Gt | Leq | Geq | Neq

type ident = string

type expr =
  | Int of int
  | Bool of bool
  | Binop of bop * expr * expr
  | If of expr * expr * expr
  | Var of ident
  | Let of ident * expr * expr

type value =
| VInt of int
| VBool of bool

let eval_op (op : bop) (v1 : value) (v2 : value) : value =
  match op, v1, v2 with
  | Add,  VInt i1, VInt i2 -> VInt (i1 + i2)
  | Sub,  VInt i1, VInt i2 -> VInt (i1 - i2)
  | Mult, VInt i1, VInt i2 -> VInt (i1 * i2)
  | Div,  VInt i1, VInt i2 -> VInt (i1 / i2)
  | Eq,   VInt i1, VInt i2 -> VBool (i1 = i2)
  | Lt,   VInt i1, VInt i2 -> VBool (i1 < i2)
  | Gt,   VInt i1, VInt i2 -> VBool (i1 > i2)
  | Leq,  VInt i1, VInt i2 -> VBool (i1 <= i2)
  | Geq,  VInt i1, VInt i2 -> VBool (i1 >= i2)
  | Neq,  VInt i1, VInt i2 -> VBool (i1 <> i2)
  | _ -> failwith "type error"


module E = Map.Make(String)

type env = value E.t

let rec eval_env (env : env) (e : expr) : value =
  match e with
  | Int n -> VInt n
  | Bool b -> VBool b
  | If (p, t, e) ->
      (match eval_env env p with
      | VBool true -> eval_env env t
      | VBool false -> eval_env env e
      | _ -> failwith "type error")
  | Binop (op, e1, e2) -> eval_op op (eval_env env e1) (eval_env env e2)
  | Let (x, e1, e2) ->
      let r = eval_env env e1 in
      let new_env = E.update x (fun _ -> Some r) env in
      eval_env new_env e2
  | Var x ->
      (match E.find_opt x env with
      | Some v -> v
      | None -> failwith ("unbound value" ^ x))

let eval : expr -> value = eval_env E.empty 


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

let rec retrieve (exp : expr) (bound_vars : string) (i : string) : (expr * string) option =
  match exp with
  | Binop(op, e1, e2) ->
    begin
      if ((string_of_expr exp) ^ bound_vars) = i then Some(exp, bound_vars) 
      else 
        try retrieve e1 bound_vars i
        with Not_found -> retrieve e2 bound_vars i
    end
  | If(p, t, e) ->
    begin
      if ((string_of_expr exp) ^ bound_vars) = i then Some(exp, bound_vars) 
      else 
        try retrieve p bound_vars i
        with Not_found -> 
          try retrieve t bound_vars i
          with Not_found -> retrieve e bound_vars i
    end
  | Let(v, s, e) ->
    begin
      if ((string_of_expr exp) ^ bound_vars) = i then Some(exp, bound_vars) 
      else 
        try retrieve s bound_vars i
        with Not_found -> retrieve e (bound_vars ^ v) i
    end
  | _ -> raise Not_found


let rec subst (exp : expr) (cur_bound : string) (bound : string) (s : expr) (w : expr) : expr = 
  match exp with
  | Binop(op, e1, e2) ->
    begin
      if exp = s && cur_bound = bound then w
      else Binop(op, subst e1 cur_bound bound s w, subst e2 cur_bound bound s w)
    end
  | If(p, t, e) ->
    begin
      if exp = s && cur_bound = bound then w
      else If(p, subst t cur_bound bound s w, subst e cur_bound bound s w)
    end
  | Let(v, s', e) ->
    begin
      if exp = s && cur_bound = bound then w
      else Let(v, subst s' (cur_bound ^ v) bound s w, subst e (cur_bound ^ v) bound s w)
    end
  | other -> other


let cse (exp : expr) : expr option = 
  match find_set exp "" with
  | Expr_set _ -> None
  | The_chosen_one tco -> 
    let (s, bound) = match retrieve exp "" tco with Some e -> e | None -> failwith "TOTAL FAILURE 1" in
    let v = "#(" ^ tco ^ ")" in
    Some (Let(v, s, subst exp "" bound s (Var v)))


(* TESTY *)

let my_env : env = E.empty 
  |> E.add "x" (VInt 1)
  |> E.add "y" (VInt 2)
  |> E.add "z" (VInt 3)

let test ex ac = match ac with Some(e) -> if eval_env my_env ex = eval_env my_env e then print_string "OK" else print_string "FAIL" | None -> print_string "None"

let ex1 = Binop(Mult, Binop(Add, Int 2, Int 2), Binop(Add, Int 2, Int 2))
let ac1 = cse ex1

let ex2 = Binop(Mult, Binop(Add, Int 3, Binop(Add, Int 2, Int 2)), Binop(Add, Int 2, Int 2))
let ac2 = cse ex2
let ex3 = Binop(Add, Binop(Add, Binop(Add, Int 1, Var "x"), Binop(Add, Int 1, Var "x")), Let("x", Int 1, Binop(Add, Int 1, Var "x")))
let ac3 = cse ex3
let ex4 = Binop(Add, Binop(Add, Int 1, Var "x"), Let("x", Int 3, Binop(Add, Int 1, Var "x")))
let ac4 = cse ex4
let ex5 = If(Binop(Gt, Var "x", Int 0), Binop(Add, Binop(Mult, Var "z", Int 10), Binop(Mult, Var "z", Int 10)), Int 0)
let ac5 = cse ex5
let ex6 = Let("x",Binop(Add,Var("x"),Var("y")),Binop(Mult,Binop(Add,Var("x"),Var("z")),Binop(Add,Var("x"),Var("z"))))
let ac6 = cse ex6
let ex7 = Let("x",Binop(Add,Var("x"),Var("y")),Binop(Mult,Binop(Add,Var("x"),Var("z")),Binop(Add,Var("x"),Var("z"))))
let ac7 = cse ex7

let _ =
  print_string "1. "; test ex1 ac1; print_endline " should be SOME";
  print_string "2. "; test ex2 ac2; print_endline " should be SOME";
  print_string "3. "; test ex3 ac3; print_endline " should be SOME";
  print_string "4. "; test ex4 ac4; print_endline " should be NONE";
  print_string "5. "; test ex5 ac5; print_endline " should be SOME";
  print_string "6. "; test ex6 ac6; print_endline " should be SOME";
  print_string "7. "; test ex7 ac7; print_endline " should be SOME";
  (*tutaj kolejne po średniku*)

