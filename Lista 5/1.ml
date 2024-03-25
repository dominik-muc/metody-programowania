let rec map f xs =
  match xs with
  | [] -> []
  | x :: xs -> f x :: map f xs

let list = [0;2;4;8]

(*Pokażę, że dla dowolnych funkcji f i g oraz listy xs zachodzi 
  P(xs) = map f (map g xs) ≡ map (fun x -> f (g x)) xs. 
   
Użyję zasady indukcji po liście. Pokażę, że zachodzi P([]).
Weźmy dowolne funkcje f i g. Wówczas:

map f (map g [])
== (Z definicji map)
map f []
== (Z definicji map)
[]

Oraz:

map (fun x -> f (g x)) []
== (Z definicji map)
[]

Załóżmy, że zachodzi P(xs). Pokażę, że wtedy zachodzi również P(a :: xs) dla dowolnego elementu a tego samego typu co xs.

L ==
map f (map g (a :: xs))
== (Z definicji map)
map f (g a :: map g xs)
== (Z definicji map)
f (g a) :: map f (map g xs)

P ==
map (fun x -> f (g x)) a :: xs
== (Z definicji map)
f (g a) :: map (fun x -> f (g x)) xs
== (Z założenia indukcyjnego)
f (g a) :: map f (map g xs)

Zatem L == P, co kończy dowód.
*)