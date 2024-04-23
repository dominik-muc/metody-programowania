type ident = string

type qbf =
  | Top
  | Bot
  | Var of ident
  | Forall of ident * qbf
  | Exists of ident * qbf
  | Not of qbf
  | Conj of qbf * qbf
  | Disj of qbf * qbf

(* Zad 6. *)

let rec subst (x : ident) (s : qbf) (f : qbf) : qbf =
  match f with
  | Var(y) -> if x = y then s else f
  | Conj(q, r) -> Conj (subst x s q, subst x s r)
  | Disj(q, r) -> Disj (subst x s q, subst x s r)
  | Not(q) -> Not(subst x s q)
  | Forall(y, r) -> if x = y then Forall(y, r) else Forall(y, subst x s r)
  | Exists(y, r) -> if x = y then Exists(y, r) else Exists(y, subst x s r) 
  | _ -> f
let rec eval (f : qbf) : bool =
  match f with
  | Top -> true
  | Bot -> false
  | Var(x) -> failwith("unbound value: " ^ x)
  | Not(f) -> not (eval f)
  | Conj(q, r) -> eval q && eval r
  | Disj(q, r) -> eval q || eval r
  | Forall(x, q) -> eval (subst x Top q) && eval (subst x Bot q)
  | Exists(x, q) -> eval (subst x Top q) || eval (subst x Bot q)

(* Zad 7. *)
  
module M = Map.Make(String)

type env = bool M.t

let rec eval_env (env : env) (f : qbf) : bool =
  match f with
  | Top -> true
  | Bot -> false
  | Var(x) -> 
    (match M.find_opt x env with
      | Some v -> v
      | None -> failwith ("unbound value" ^ x))
  | Not(f) -> not (eval_env env f)
  | Conj(q, r) -> eval_env env q && eval_env env r
  | Disj(q, r) -> eval_env env q || eval_env env r
  | Forall(x, q) -> eval_env (M.add x true env) q && eval_env (M.add x false env) q
  | Exists(x, q) -> eval_env (M.add x true env) q || eval_env (M.add x false env) q