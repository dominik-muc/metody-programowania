(* abstract syntax tree *)

type bop = Mult | Div | Add | Sub | Eq | Ls | LsE | Gt | GtE | NEq

type expr =
  | Int of int
  | Bool of bool
  | Binop of bop * expr * expr
  | If of expr * expr * expr
  | And of expr * expr
  | Or of expr * expr
                               
