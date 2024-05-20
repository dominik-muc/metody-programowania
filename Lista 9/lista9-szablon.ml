type expr =
  | Int of int
  | Add of expr * expr
  | Mult of expr * expr

let rec eval (e : expr) : int =
  match e with
  | Int n -> n
  | Add (e1, e2) -> eval e1 + eval e2
  | Mult (e1, e2) -> eval e1 * eval e2

type rpn_cmd =
  | Push of int
  | RAdd
  | RMult

type rpn = rpn_cmd list

let rec to_rpn (e : expr) : rpn =
  match e with
  | Int n -> [Push n]
  | Add (e1, e2) -> to_rpn e1 @ to_rpn e2 @ [RAdd]
  | Mult (e1, e2) -> to_rpn e1 @ to_rpn e2 @ [RMult]

let rec eval_rpn (r : rpn) (s : int list) : int =
  match r, s with
  | [], [n] -> n
  | Push n :: r', _ -> eval_rpn r' (n :: s)
  | RAdd :: r', n1 :: n2 :: s' -> eval_rpn r' (n2 + n1 :: s')
  | RMult :: r', n1 :: n2 :: s' -> eval_rpn r' (n2 * n1 :: s')
  | _,_ -> failwith "error!"

(* ZAD 1 *)

(*

Udowodnij, że dla każdego e zachodzi eval_rpn (to_rpn e) [] == eval e

Indukcja dla expr:
  1) Jeżeli twierdzenie ɸ jest spełnione dla (Int n), gdzie n jest dowolną liczbą całkowitą oraz
  2) Jeżeli twierdzenie ɸ jest spełnione dla e1, e2 : expr, to jest spełnione także dla Add (e1, e2) oraz Mult(e1, e2)
to ɸ jest spełnione dla dowolnego wyrażenia e : expr

Udowdonimy silniejsze twierdzenie: eval_rpn ((to_rpn e) @ r) s == eval_rpn r ([eval e] :: s)

1. Baza indukcji: Udowodnię, że dla dowolnego n zachodzi eval_rpn (to_rpn (Int n) @ r) s = eval_rpn r ([eval (Int n)] :: s)
  eval_rpn (to_rpn (Int n) @ r) s
  == Z def. to_rpn
  eval_rpn ([Push n] @ r) s
  == Z def. eval_rpn
  eval_rpn r ([n] :: s)
  == Z def. eval
  eval_rpn r ([eval (Int n)] :: s)

2. Założmy, że e1, e2 spełniają twierdzenie. Wówczas:
  1) Udowodnię, że Add (e1, e2) spełnia twierdzenie

    eval_rpn ((to_rpn Add(e1, e2)) @ r) s
    == Z def. to_rpn
    eval_rpn (to_rpn e1 @ to_rpn e2 @ [RAdd] @ r) s
    == Z zał. indukcyjnego
    eval_rpn (to_rpn e2 @ [RAdd] @ r) ([eval e1] :: s)
    == Z zał. indukcyjnego
    eval_rpn ([RAdd] @ r) ([eval e2] :: [eval e1] :: s)
    == Z def. eval_rpn
    eval_rpn r ([eval e1 + eval e2] :: s)
    == Z def. eval
    eval_rpn r ([eval Add(e1, e2)] :: s)

  2) Analogicznie.

Teraz za r i s wstawmy []:
Wówczas:
  eval_rpn (to_rpn e :: []) [] == eval_rpn [] ([eval e] :: []]) == eval e

Co kończy dowód.

*)

(* ZAD 2 *)

let rec from_rpn (r : rpn) (s : expr list) : expr =
  match r, s with
  | [], [e] -> e
  | Push n :: r', s -> from_rpn r' ((Int n) :: s)
  | RAdd :: r', e1 :: e2 :: s' -> from_rpn r' ((Add(e2, e1)) :: s')
  | RMult :: r', e1 :: e2 :: s' -> from_rpn r' ((Mult(e2, e1)) :: s')
  | _ -> failwith "syntax error"
  
(* ZAD 3 *)

(* porównaj zad 3. z listy 6. *)

let max = 100

let rec random_expr (max_depth : int) : expr =
  if (max_depth = 0) then Int (Random.int max) else
  let r = Random.int 3 in
  match r with
  | 0 -> Int (Random.int max)
  | 1 -> Add (random_expr (max_depth - 1), random_expr (max_depth - 1))
  | _ -> Mult(random_expr (max_depth - 1), random_expr (max_depth - 1))

let rec test (max_depth : int) (sample : int) : bool =
  if sample = 0 then true else
  let e = random_expr max_depth in
  let result = from_rpn (to_rpn e) [] in
  result = e && test max_depth (sample - 1)

(* ZAD 4 *)
let rec from_rpn_bad (r : rpn) (s : expr list) : expr =
  match r, s with
  | [], [e] -> e
  | Push n :: r', s -> from_rpn_bad r' ((Int n) :: s)
  | RAdd :: r', e1 :: e2 :: s' -> from_rpn_bad r' ((Add(e1, Int 0)) :: s')
  | RMult :: r', e1 :: e2 :: s' -> from_rpn_bad r' ((Mult(e1, e2)) :: s')
  | _ -> failwith "syntax error"

let rec test_ce (max_depth : int) (sample : int) : expr option =
  if sample = 0 then None else
  let e = random_expr max_depth in
  let result = from_rpn_bad (to_rpn e) [] in
  if result = e then test_ce max_depth (sample - 1) else Some result

(* ZAD 5 *)

type bop = Mult | Div | Add | Sub | Eq | Lt | Gt | Leq | Geq | Neq

module T = struct

type cmd =
  | PushInt of int
  | PushBool of bool
  | Prim of bop
  | Jmp of string
  | JmpFalse of string
  | Grab
  | Access of int
  | EndLet
  | PushClo of string
  | Call of string
  | Return
  | Lbl of string

end
