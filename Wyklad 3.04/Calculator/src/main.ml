open Ast

let parse (s : string) : expr =
  Parser.prog Lexer.read (Lexing.from_string s)

let eval_op (op: bop) : int -> int -> int =
  match op with
  | ADD -> (+)
  | SUB -> (-)
  | MULT -> ( * )
  | DIV -> (/)

let rec eval (e: expr) : int = 
  match e with
  | Int n -> n
  | Binop (op, e1, e2) -> eval_op op (eval e1) (eval e2)

let interp (s: string) : int =
  eval (parse s)