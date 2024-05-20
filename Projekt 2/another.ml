type bop = Mult | Div | Add | Sub | Eq | Lt | Gt | Leq | Geq | Neq

type ident = string

type expr =
  | Int of int
  | Bool of bool
  | Binop of bop * expr * expr
  | If of expr * expr * expr
  | Var of ident
  | Let of ident * expr * expr

module MySet = Set.Make(expr)

let rec cse exp : expr option = function
  | Binop(op, e1, e2) -> 
    begin match cse e1, cse e2 with
    | 
    end