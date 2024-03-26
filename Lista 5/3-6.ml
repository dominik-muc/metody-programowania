type 'v nnf =
| NNFLit  of   bool * 'v
| NNFConj of 'v nnf * 'v nnf
| NNFDisj of 'v nnf * 'v nnf

(*Zasada indukcji dla nnf.

Weźmy dowolną własność P.
Jeżeli dla dowolnej zmiennej bool b i zmiennej zdaniowej v zachodzi P(NNFLit (b, v)) oraz
  jeżeli dla dowolnych formuł q i r spełniajacych P(q) i P(r) zachodzi P(NNFConj (q, r)) oraz P(NNFDisj (q, r)),
to dla dowolnej formuły s zachodzi P(s).

*)

let rec neg_nnf f =
  match f with
  | NNFLit  (b, v) -> NNFLit  (not b, v)
  | NNFConj (q, r) -> NNFDisj (neg_nnf q, neg_nnf r)
  | NNFDisj (q, r) -> NNFConj (neg_nnf q, neg_nnf r)

let rec f_to_string f =
  match f with
  | NNFLit (b, v) -> (if not b then "¬" else "") ^ v
  | NNFConj (q, r) -> "(" ^ f_to_string q ^ " ∧ " ^ f_to_string r ^ ")"
  | NNFDisj (q, r) -> "(" ^ f_to_string q ^ " ∨ " ^ f_to_string r ^ ")"

let f1 = NNFDisj(NNFLit (true, "q"), NNFLit (false, "r"))

(* Dowód: P(f) = neg_nnf (neg_nnf f) == f

1. Baza indukcji
Pokażę, że zachodzi P(NFFLit(b, v)), dla dowolnej zmiennej boolowskiej b i zmiennej zdaniowej v.
  1)  b = true
      neg_nnf (neg_nnf (true, v))
      == (Z definicji neg_nnf)
      neg_nnf (false, v)
      == (Z definicji neg_nnf)
      (true, v)
  2)  b = false analogicznie

2. Krok indukcyjny
Załóżmy, że dla dowolnych formuł q, r zachodzi P(q) oraz P(r).
Pokaże, że zachodzi także P(NNFConj(q, r)) oraz P(NNFDisj(q, r))
  1)  P(NNFConj(q, r))
      neg_nnf (neg_nnf NNFConj(q, r))
      == (Z definicji neg_nnf)
      neg_nnf (NNFDisj(neg_nnf q, neg_nnf r))
      == (Z definicji neg_nnf)
      NNFConj(neg_nnf (neg_nnf q), neg_nnf (neg_nnf r))
      == (Z założenia indukcyjnego)
      NNFConj(q, r)
  2)  P(NNfDisj(q, r)) analogicznie

Co kończy dowód.
*)

let rec eval_nnf s f =
  match f with
  | NNFLit (b, v) -> if b then (s v) else (not (s v))
  | NNFConj (q, r) -> (eval_nnf s q) && (eval_nnf s r)
  | NNFDisj (q, r) -> (eval_nnf s q) || (eval_nnf s r)

let a = 5
(* Dowód: P(f) = eval_nnf s (neg_nnf f) ≡ not (eval_nnf s f)

1. Baza indukcji
Pokażę, że dla dowolnego wartościowania s, zachodzi P(NNFLit (b, v))
  1)  b = true
      eval_nnf s (neg_nnf (true, v))
      == (Z definicji neg_nnf)
      eval_nnf s (false, v)
      == (Z definicji eval_nnf)
      not (s v)

      not (eval_nnf s (true, v))
      == (Z definicji eval_nnf)
      not (s v)
  2) b = false analogicznie

2. Krok indukcyjny
Załóżmy, że zachodzi P(q) i P(r). Pokażmy, że zachodzi P(NNFConj (q, r)) oraz P(NNFDisj (q, r))

  1)  P(NNFConj (q, r))
      eval_nnf s (neg_nnf NNFConj (q, r))
      == (Z definicji neg_nnf)
      eval_nnf s (NNFDisj (neg_nnf q, neg_nnf r))
      == (Z definicji eval_nnf)
      (eval_nnf s (neg_nnf q)) && (eval_nnf s (neg_nnf r))
      
      not (eval_nnf s NNFDisj (q, r))
      == (Z definicji eval_nnf)
      not ((eval_nnf s q) || (eval_nnf s r))
      == (Z praw De morgana)
      (not eval_nnf s q) && (not eval_nnf s r)
      == (Z założenia indukcyjnego)
      (eval_nnf s (neg_nnf q)) && (eval_nnf s (neg_nnf r))
  2)  P(NNFDisj (q, r)) analogicznie

Co kończy dowód.
*)

type 'v formula =
| Var of 'v
| Neg of 'v formula
| Conj of 'v formula * 'v formula
| Disj of 'v formula * 'v formula

let rec to_nnf f =
  match f with
  | Var v -> NNFLit(true, v)
  | Neg p -> neg_nnf (to_nnf p)
  | Conj (p, q) -> NNFConj(to_nnf p, to_nnf q)
  | Disj (p, q) -> NNFDisj(to_nnf p, to_nnf q)
