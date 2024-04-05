%{
    open Ast
%}

%token <int> INT
%token TIMES
%token DIV
%token PLUS
%token MINUS
%token LPAREN
%token RPAREN
%token EOF

%start <Ast.expr> prog

%left PLUS MINUS
%left TIMES DIV

%%

prog:
    | e = expr; EOF { e }
    ;

expr:
    | i = INT { Int i }
    | e1 = expr; PLUS; e2 = expr { Binop(ADD, e1, e2) }
    | e1 = expr; MINUS; e2 = expr { Binop(SUB, e1, e2) }
    | e1 = expr; TIMES; e2 = expr { Binop(MULT, e1, e2) }
    | e1 = expr; DIV; e2 = expr { Binop(DIV, e1, e2) }
    | LPAREN; e = expr; RPAREN { e }