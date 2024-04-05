(* Ast*)
type bop = ADD | SUB | MULT | DIV

type expr =
  | Int of int
  | Binop of bop * expr * expr