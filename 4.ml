let f x y z = if x > y then (if y > z then x*x+y*y else x*x+z*z) else (if z > x then z*z+y*y else x*x+y*y);;

let wynik = f 3 4 2;;

print_endline (string_of_int wynik);;
