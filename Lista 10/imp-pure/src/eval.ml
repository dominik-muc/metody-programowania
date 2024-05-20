open Ast

module E = Map.Make(String)

module H = List

type env = int E.t

type heap = value H.t

let rec replace_nth l n v =
  match l with
  | [] -> []
  | head :: tail ->
    if n = 0 then v :: tail
    else head :: replace_nth tail (n - 1) v

let rec add_n xs n v =
  if n = 0 then xs
  else add_n (xs @ [v]) (n - 1) v

let lookup_var env heap x =
  match E.find_opt x env with
  | Some n -> 
    (match H.nth_opt heap n with
    | Some v -> v
    | None -> failwith ("Invalid pointer: " ^ (string_of_int n)))
  | None   -> failwith ("Unbound variable " ^ x)

let assign_var env heap x v : heap =
  match E.find_opt x env with
  | Some n -> 
    (match H.nth_opt heap n with
    | Some _ -> replace_nth heap n v
    | None -> (add_n heap (n - H.length heap)) Null) @ [v]
  | None   -> failwith ("Unbound variable " ^ x)

let declare_var env x : env =
  E.add x (E.cardinal env) env

let eval_binop op e1 e2 : value =
  match op with
  | Mul -> Integer (e1 * e2)
  | Div -> Integer (e1 / e2)
  | Add -> Integer (e1 + e2)
  | Sub -> Integer (e1 - e2)
  | Eq  -> Boolean (e1 = e2)
  | Neq -> Boolean (e1 <> e2)
  | Lt  -> Boolean (e1 < e2)
  | Gt  -> Boolean (e1 > e2)
  | Leq -> Boolean (e1 <= e2)
  | Geq -> Boolean (e1 >= e2)

let rec eval_exp env heap e =
  match e with
  | Val v -> v
  | Var x -> 
    (match lookup_var env heap x with 
    | Null -> failwith "null-pointer dereference"
    | v -> v)
  | Binop(op, e1, e2) ->
    (match eval_exp env heap e1, eval_exp env heap e2 with
    | Integer n, Integer m -> eval_binop op n m
    | _ -> failwith "type error")
  | And(e1, e2) ->
    (match eval_exp env heap e1 with
    | Boolean p -> if not p then Boolean false else (match eval_exp env heap e2 with Boolean q -> Boolean (p && q) | _ -> failwith "type_error")
    | _ -> failwith "type error and")
  | Or(e1, e2) ->
    (match eval_exp env heap e1 with
    | Boolean p -> if p then Boolean true else (match eval_exp env heap e2 with Boolean q -> Boolean (p || q) | _ -> failwith "type_error")
    | _ -> failwith "type error or")
  | Not e -> 
    (match eval_exp env heap e with
    | Boolean p -> Boolean (not p)
    | _ -> failwith "type error")
  | _ -> failwith "not implemented"
  

let rec eval_stmt env heap s : heap =
  match s with
  | Block (vs, ss) -> 
    let local_env = List.fold_left declare_var env vs in
    let rec helper new_heap ss =
      match ss with
      | [] -> new_heap
      | x :: xs -> helper (eval_stmt local_env new_heap x) xs
    in helper heap ss
  | Assign(x, e) -> assign_var env heap x (eval_exp env heap e)
  | If(e, s1, s2) ->
    (match eval_exp env heap e with
    | Boolean b -> if b then eval_stmt env heap s1 else eval_stmt env heap s2
    | _ -> failwith "type error")
  | While(e, s) ->
    eval_while env heap e s
  | Read x ->
    Integer (read_line () |> int_of_string) |> assign_var env heap x
  | Write e ->
    (match eval_exp env heap e with 
    | Integer n -> string_of_int n |> print_endline
    | Boolean b -> string_of_bool b |> print_endline
    | _ -> failwith "not writeable");
    heap

and eval_while env heap e s =
  match eval_exp env heap e with
  | Boolean b -> 
    if b then eval_while env (eval_stmt env heap s) e s
    else heap
  | _ -> failwith "type error"

let run_prog (xs, stmt) =
  let env = List.fold_left declare_var E.empty xs in
  let _ : heap = eval_stmt env [Null] stmt in
  ()