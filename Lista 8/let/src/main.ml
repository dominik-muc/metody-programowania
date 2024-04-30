open Ast

let parse (s : string) : expr =
  Parser.prog Lexer.read (Lexing.from_string s)

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


(* Evaluation via substitution *)

module Subst = struct

let rec subst (x : ident) (s : expr) (e : expr) : expr =
  match e with
  | Binop (op, e1, e2) -> Binop (op, subst x s e1, subst x s e2)
  | If (p, t, e) -> If (subst x s p, subst x s t, subst x s e)
  | Var y -> if x = y
               then s
               else e
  | Let (y, e1, e2) -> if x = y
                         then Let (y, subst x s e1, e2)
                         else Let (y, subst x s e1, subst x s e2)
  | _ -> e
  
let expr_of_value (v : value) : expr =
  match v with
  | VInt a -> Int a
  | VBool a -> Bool a

let rec eval (e : expr) : value =
  match e with
  | Int n -> VInt n
  | Bool b -> VBool b
  | If (p, t, e) ->
      (match eval p with
      | VBool true -> eval t
      | VBool false -> eval e
      | _ -> failwith "type error")
  | Binop (And, e1, e2) ->
      (match eval e1 with
      | VBool true -> eval e2
      | VBool false -> VBool false
      | _ -> failwith "type error")
  | Binop (Or, e1, e2) ->
      (match eval e1 with
      | VBool false -> eval e2
      | VBool true -> VBool true
      | _ -> failwith "type error")
  | Binop (op, e1, e2) -> eval_op op (eval e1) (eval e2)
  | Let (x, e1, e2) -> let s = expr_of_value (eval e1) in
                       eval (subst x s e2)
  | Var x -> failwith ("unbound value " ^ x)

let interp (s : string) : value =
  eval (parse s)

end


(* Evaluation via environments *)

module Env = struct

module M = Map.Make(String)

type env = value M.t

let rec eval_env (env : env) (e : expr) : value =
  match e with
  | Int n -> VInt n
  | Bool b -> VBool b
  | If (p, t, e) ->
      (match eval_env env p with
      | VBool true -> eval_env env t
      | VBool false -> eval_env env e
      | _ -> failwith "type error")
  | Binop (And, e1, e2) ->
      (match eval_env env e1 with
      | VBool true -> eval_env env e2
      | VBool false -> VBool false
      | _ -> failwith "type error")
  | Binop (Or, e1, e2) ->
      (match eval_env env e1 with
      | VBool false -> eval_env env e2
      | VBool true -> VBool true
      | _ -> failwith "type error")
  | Binop (op, e1, e2) -> eval_op op (eval_env env e1) (eval_env env e2)
  | Let (x, e1, e2) ->
      let r = eval_env env e1 in
      let new_env = M.update x (fun _ -> Some r) env in
      eval_env new_env e2
  | Var x ->
      (match M.find_opt x env with
      | Some v -> v
      | None -> failwith ("unbound value" ^ x))

let eval : expr -> value = eval_env M.empty 

let interp (s : string) : value =
  eval (parse s)

end

module Zad1 = struct

module M = Map.Make(String)
type env = expr M.t

let expr_of_value value = 
  match value with
  | VInt n -> Int n
  | VBool b -> Bool b

let rec cp (e: expr) (env: env) : expr =
  match e with
  | Binop(op, e1, e2) -> 
    (match (cp e1 env, cp e2 env) with
    | (Int n, Int m) -> expr_of_value (eval_op op (VInt n) (VInt m))
    | (any, other) -> Binop(op, any, other))
  | Var x -> (match M.find_opt x env with Some v -> v | None -> Var x)
  | Let(x, e1, e2) -> (match cp e1 env with
    | Int a -> cp e2 (M.add x (Int a) env)
    | Bool b -> cp e2 (M.add x (Bool b) env)
    | e1' -> Let(x, e1', (cp e2 (M.add x (Var x) env))))
  | If(p, t, el) -> (match cp p env with 
    | Bool(true) -> cp t env
    | Bool(false) -> cp el env
    | other -> If(other, cp t env, cp el env))
  | _ -> e

let cp_empty (e: expr) = cp e M.empty 

end

module Zad2 = struct
  
module M = List
type env = ident M.t

let find_index var env =
  let rec helper it env = 
    match env with
    | [] -> None
    | x :: xs -> if x = var then Some it else helper (it + 1) xs
  in helper 0 env

let rec indexed (e: expr) (env: env) : expr = 
  match e with
  | Var x -> (match find_index x env with Some i -> Var ("var"^(string_of_int i)) | _ -> Var x)
  | Let(x, e1, e2) -> Let("var", indexed e1 env, indexed e2 (x :: env))
  | Binop(op, e1, e2) -> Binop(op, indexed e1 env, indexed e2 env)
  | If(p, t, el) -> If(indexed p env, indexed t env, indexed el env)
  | _ -> e

let alpha_equiv (e1 : expr) (e2: expr) : bool = indexed e1 [] = indexed e2 []

end

module Zad3 = struct

module M = Map.Make(String)
type env = expr M.t

let rec rename (e: expr)(env: env)(path: string) : expr =
  match e with
  | Var x -> (match M.find_opt x env with Some v -> v | None -> Var (path ^ x))
  | Let(x, e1, e2) -> Let(path, rename e1 env (path ^ "L"), rename e2 (M.add x (Var path) env) (path ^ "R"))
  | Binop(op, e1, e2) -> Binop(op, rename e1 env (path ^ "L"), rename e2 env (path ^ "R"))
  | If(p, th, el) -> If(rename p env (path ^ "P"), rename th env (path ^ "T"), rename el env (path ^ "E"))
  | _ -> e

let rename_empty (e: expr) : expr = rename e M.empty "#"

end