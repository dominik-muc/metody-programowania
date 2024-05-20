%token <int> INT
%token <string> IDENT
%token TIMES DIV PLUS MINUS
%token EQ NEQ LT GT LEQ GEQ
%token AND OR NOT
%token ASSIGN LPAREN RPAREN
%token BEGIN END IF THEN ELSE WHILE DO READ WRITE VAR SKIP TRUE FALSE
%token EOF

%start <Ast.prog> prog

%{
open Ast
%}

%right OR
%right AND
%right NOT
%nonassoc EQ NEQ LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIV

%%

expr:
  | n=INT   { Val (Integer n) }
  | x=IDENT { Var x }
  | LPAREN; e=expr; RPAREN { e }
  | e1=expr; PLUS;  e2=expr { Binop(Add, e1, e2) }
  | e1=expr; MINUS; e2=expr { Binop(Sub, e1, e2) }
  | e1=expr; TIMES; e2=expr { Binop(Mul, e1, e2) }
  | e1=expr; DIV;   e2=expr { Binop(Div, e1, e2) }
  | e1=expr; EQ;   e2=expr { Binop(Eq, e1, e2) }
  | e1=expr; NEQ;   e2=expr { Binop(Neq, e1, e2) }
  | e1=expr; LT;   e2=expr { Binop(Lt, e1, e2) }
  | e1=expr; GT;   e2=expr { Binop(Gt, e1, e2) }
  | e1=expr; LEQ;   e2=expr { Binop(Leq, e1, e2) }
  | e1=expr; GEQ;   e2=expr { Binop(Geq, e1, e2) }
  | TRUE  { Val (Boolean true)  }
  | FALSE { Val (Boolean false) }
  | LPAREN; e=expr; RPAREN { e }
  | e1=expr; AND; e2=expr { And(e1, e2) }
  | e1=expr; OR;  e2=expr { Or(e1, e2)  }
  | NOT; e=expr { Not e }
  ;

stmt:
  | SKIP { Block ([], []) }
  | BEGIN; vs=list(var_decl); s=list(stmt); END { Block (vs, s) }
  | x=IDENT; ASSIGN e=expr { Assign(x, e) }
  | IF; e=expr; THEN; s1=stmt; ELSE; s2=stmt { If(e, s1, s2) }
  | WHILE; e=expr; DO; s=stmt { While(e, s) }
  | READ; x=IDENT { Read x }
  | WRITE; e=expr { Write e }
  ;

var_decl:
  | VAR; x=IDENT { x }
  ;

prog:
  | vs=list(var_decl); s=stmt; EOF { (vs, s) }
  ;
