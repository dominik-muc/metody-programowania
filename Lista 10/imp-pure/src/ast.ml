type binop = Mul | Div | Add | Sub | Eq | Neq | Lt | Gt | Leq | Geq

type var = string

type value =
  | Integer of int
  | Boolean of bool
  | Null

type exp =
  | Val of value
  | Var   of var
  | Incr  of var
  | Binop of binop * exp * exp
  | And  of exp * exp
  | Or   of exp * exp
  | Not  of exp

type stmt =
  | Block of var list * stmt list
  | Assign of var * exp
  | If    of exp * stmt * stmt
  | While of exp * stmt
  | Read  of var
  | Write of exp

type prog = var list * stmt
