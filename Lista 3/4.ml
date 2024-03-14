let empty_set = (fun a -> false)

let singleton a = (fun x -> x = a)

let in_set a s = s a

let union s t = (fun a -> s a || t a)

let intersect s t = (fun a -> s a && t a)

let even_set = (fun a -> a mod 2 = 0)