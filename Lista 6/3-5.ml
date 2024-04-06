(* ---------- Zad 3 ---------- *)

module Zad3 = struct

open List

type 'a symbol =
  | T of string
  | N of 'a

type 'a grammar = ('a * ('a symbol list) list) list

let rec generate (g : 'a grammar) (s : 'a) : string = 
  let l = assoc s g in
  let c = nth l (Random.int (length l)) in
  let rec helper xs =
    match xs with
    | [] -> [""]
    | T a :: xs -> a :: helper xs
    | N a :: xs -> generate g a :: helper xs
  in String.concat "" (helper c)


let pol : string grammar =
  [ "zdanie", [[N "grupa-podmiotu"; N "grupa-orzeczenia"]]
  ; "grupa-podmiotu", [[N "przydawka"; N "podmiot"]]
  ; "grupa-orzeczenia", [[N "orzeczenie"; N "dopelnienie"]]
  ; "przydawka", [[T "Piękny "];
                  [T "Bogaty "];
                  [T "Wesoły "]]
  ; "podmiot", [[T "policjant "];
                [T "student "];
                [T "piekarz "]]
  ; "orzeczenie", [[T "zjadł "];
                   [T "pokochał "];
                   [T "zobaczył "]]
  ; "dopelnienie", [[T "zupę."];
                    [T "studentkę."];
                    [T "sam siebie."];
                    [T "instytut informatyki."]]]

(* generate pol "zdanie" *)

let expr : unit grammar =
  [(), [[N (); T "+"; N ()];
        [N (); T "*"; N ()];
        [T "("; N (); T ")"];
        [T "1"];
        [T "2"]]]

(* generate expr () *)

let c_grammar : string grammar =
  [ "program", [[N "declaration_list"; N "function_list"]]
  ; "declaration_list", [[N "declaration"; N "declaration_list"];
                         []]
  ; "declaration", [[N "type"; N "identifier"; T ";"]]
  ; "type", [[T "int "];
             [T "float "];
             [T "char "]]
  ; "identifier", [[T "x"];
                   [T "y"];
                   [T "z"]]
  ; "function_list", [[N "function"; N "function_list"];
                      []]
  ; "function", [[N "type"; N "identifier"; T "("; N "parameter_list"; T ")"; T "{"; N "statement_list"; T "}"]]
  ; "parameter_list", [[N "type"; N "identifier"; N "parameter_list_tail"];
                       []]
  ; "parameter_list_tail", [[T ","; N "type"; N "identifier"; N "parameter_list_tail"];
                            []]
  ; "statement_list", [[N "statement"; N "statement_list"];
                       []]
  ; "statement", [[N "assignment_statement"];
                  [N "return_statement"];
                  [N "expression_statement"]]
  ; "assignment_statement", [[N "identifier"; T "="; N "expression"; T ";"]]
  ; "return_statement", [[T "return "; N "expression"; T ";"]]
  ; "expression_statement", [[N "expression"; T ";"];
                             [T ";"]]
  ; "expression", [[N "simple_expression"];
                   [N "identifier"; T "="; N "expression"]]
  ; "simple_expression", [[N "term"; N "simple_expression_tail"]]
  ; "simple_expression_tail", [[N "addop"; N "term"; N "simple_expression_tail"];
                               []]
  ; "term", [[N "factor"; N "term_tail"]]
  ; "term_tail", [[N "mulop"; N "factor"; N "term_tail"];
                  []]
  ; "factor", [[T "("; N "expression"; T ")"];
               [N "identifier"];
               [N "number"]]
  ; "addop", [[T "+"]; [T "-"]]
  ; "mulop", [[T "*"]; [T "/"]]
  ; "number", [[T "1"]; [T "2"]; [T "3"]]
  ; "identifier", [[T "a"]; [T "b"]; [T "c"]]
  ]

let ocaml_grammar : string grammar =
  [ "program", [[N "module_definition"; N "program"];
                [N "type_definition"; N "program"];
                [N "let_definition"; N "program"];
                []]
  ; "module_definition", [[T "module "; N "capitalized_identifier"; T " = struct "; N "program"; T "end"]]
  ; "type_definition", [[T "type "; N "lowercase_identifier"; T " = "; N "type_body"]]
  ; "type_body", [[N "constructor_definition"; N "type_body_tail"];
                  [N "record_definition"]]
  ; "type_body_tail", [[T "| "; N "constructor_definition"; N "type_body_tail"];
                        []]
  ; "constructor_definition", [[N "capitalized_identifier"; N "of_types"]]
  ; "of_types", [[T " of "; N "type_expression"; N "of_types_tail"];
                  []]
  ; "of_types_tail", [[T " * "; N "type_expression"; N "of_types_tail"];
                      []]
  ; "record_definition", [[T "{"; N "record_field"; N "record_definition_tail"; T "}"]]
  ; "record_definition_tail", [[T "; "; N "record_field"; N "record_definition_tail"];
                                []]
  ; "record_field", [[N "lowercase_identifier"; T " : "; N "type_expression"]]
  ; "let_definition", [[T "let "; N "lowercase_identifier"; T " = "; N "expression"]]
  ; "expression", [[N "function_call"];
                    [N "if_expression"];
                    [N "match_expression"];
                    [N "simple_expression"]]
  ; "function_call", [[N "lowercase_identifier"; T " "; N "expression"]]
  ; "if_expression", [[T "if "; N "expression"; T " then "; N "expression"; T " else "; N "expression"]]
  ; "match_expression", [[T "match "; N "expression"; T " with "; N "pattern_matching"]]
  ; "pattern_matching", [[N "pattern"; T " -> "; N "expression"; N "pattern_matching_tail"]]
  ; "pattern_matching_tail", [[N "pattern"; T " -> "; N "expression"; N "pattern_matching_tail"];
                              []]
  ; "pattern", [[N "simple_expression"]; [T "_"]]
  ; "simple_expression", [[T "true"]; [T "false"]; [N "number"]; [N "lowercase_identifier"]]
  ; "type_expression", [[T "int"]; [T "bool"]; [T "string"]; [T "float"]]
  ; "number", [[T "1"]; [T "2"]; [T "3"]]
  ; "lowercase_identifier", [[T "x"]; [T "y"]; [T "z"]]
  ; "capitalized_identifier", [[T "Option"]; [T "Result"]; [T "List"]]
  ]

end

(* ---------- Zad 4 i 5 ---------- *)

(* List.of_seq (String.to_seq str) *)

module Zad45 = struct

let parens_ok (i : string) = 
  let match_opening a =
    match a with
    | '(' -> ')'
    | '{' -> '}'
    | '[' -> ']'
    | _ -> '?' 
  in
  let ls = List.of_seq (String.to_seq i) in
  let rec helper xs stack = 
    match xs with
    | ('(' | '{' | '[') as a :: xs -> helper xs (a :: stack)
    | (')' | '}' | ']') as a :: xs ->
      (match stack with
      | y :: ys -> if match_opening y = a then helper xs ys else ('f' :: stack)
      | _ -> ('f' :: stack))
    | _ -> stack
    in (helper ls [])

end

