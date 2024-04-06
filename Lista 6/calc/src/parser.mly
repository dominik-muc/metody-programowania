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
%token EQ
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token LESS
%token LESSEQ
%token GREATER
%token GREATEREQ
%token NOTEQ
%token AND
%token OR
%token EOF

%start <Ast.expr> prog

%nonassoc ELSE
%nonassoc EQ
%left PLUS MINUS
%left TIMES DIV

%%

prog:
  | e = expr; EOF { e }
  ;

expr:
  | i = INT { Int i }
  | e1 = expr; PLUS; e2 = expr { Binop(Add, e1, e2) }
  | e1 = expr; MINUS; e2 = expr { Binop(Sub, e1, e2) }
  | e1 = expr; DIV; e2 = expr { Binop(Div, e1, e2) }
  | e1 = expr; TIMES; e2 = expr { Binop(Mult, e1, e2) }
  | LPAREN; e = expr; RPAREN { e }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | e1 = expr; EQ; e2 = expr { Binop(Eq, e1, e2) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If(e1, e2, e3) }
  | e1 = expr; LESS; e2 = expr { Binop(Ls, e1, e2) }
  | e1 = expr; LESSEQ; e2 = expr { Binop(LsE, e1, e2) }
  | e1 = expr; GREATER; e2 = expr { Binop(Gt, e1, e2) }
  | e1 = expr; GREATEREQ; e2 = expr { Binop(GtE, e1, e2) }
  | e1 = expr; NOTEQ; e2 = expr { Binop(NEq, e1, e2) }
  | e1 = expr; AND; e2 = expr { And(e1, e2) }
  | e1 = expr; OR; e2 = expr { Or(e1, e2) }
  ;
