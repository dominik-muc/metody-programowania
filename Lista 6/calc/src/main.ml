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
  | Ls,   VInt i1, VInt i2 -> VBool (i1 < i2)
  | LsE,  VInt i1, VInt i2 -> VBool (i1 <= i2)
  | Gt,   VInt i1, VInt i2 -> VBool (i1 > i2)
  | GtE,  VInt i1, VInt i2 -> VBool (i1 >= i2)
  | NEq,  VInt i1, VInt i2 -> VBool (i1 <> i2)
  | _ -> failwith "type error"

let rec eval (e : expr) : value =
  match e with
  | Int n -> VInt n
  | Bool b -> VBool b
  | If (p, t, e) ->
      (match eval p with
      | VBool true -> eval t
      | VBool false -> eval e
      | _ -> failwith "type error")
  | And (e1, e2) -> 
      (match eval e1 with
      | VBool false -> VBool false
      | VBool true -> eval e2
      | _ -> failwith "type error")
  | Or (e1, e2) ->
      (match eval e1 with
      | VBool true -> VBool true
      | VBool false -> eval e2
      | _ -> failwith "type error")
  | Binop (op, e1, e2) -> eval_op op (eval e1) (eval e2)

let interp (s : string) : value =
  eval (parse s)

let eval_op_mod5 (op : bop) (v1 : value) (v2 : value) : value =
  match op, v1, v2 with
  | Add,  VInt i1, VInt i2 -> VInt ((i1 + i2) mod 5)
  | Sub,  VInt i1, VInt i2 -> VInt ((i1 - i2) mod 5)
  | Mult, VInt i1, VInt i2 -> VInt ((i1 * i2) mod 5)
  | Div,  VInt i1, VInt i2 -> VInt ((i1 / i2) mod 5)
  | Eq,   VInt i1, VInt i2 -> VBool ((i1 mod 5) = (i2 mod 5))
  | _ -> failwith "type error"

let rec eval_mod5 (e : expr) : value =
  match e with
  | Int n -> VInt (n mod 5)
  | Bool b -> VBool b
  | If (p, t, e) ->
      (match eval_mod5 p with
      | VBool true -> eval_mod5 t
      | VBool false -> eval_mod5 e
      | _ -> failwith "type error") 
  | Binop (op, e1, e2) -> eval_op_mod5 op (eval_mod5 e1) (eval_mod5 e2)
  | _ -> failwith "not implemented or type error"

let interp_mod5 (s : string) : value =
  eval_mod5 (parse s)

