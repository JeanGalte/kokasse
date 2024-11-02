%{
    open Ast
 %}

%token EOF

%token LB RB LP RP

%token PRINT

%token FUN

%token <int> INT

%token NOT

%token PLUS MOINS FOIS DIV

%left PLUS MOINS
%left FOIS DIV
%nonassoc NOT

%token <string> IDENT

%type <Ast.program> prog

%start prog

%%

//Un programme est une suite de définitions de fonctions nécessairement non vide 
prog:
    p = fun_def+
    EOF
    { p }
;

fun_def:
    FUN name=ident LP RP LB e=expr RB
    { (name, { args = []; ret_type = None; body=e}) }
;

expr:
  | b = bexpr { b }
;

bexpr:
  | a = atom {Atom a}
  | u=unop be = bexpr { Unop (u, be) }
  | be1 = bexpr b = binop be2 = bexpr { Binop (b, be1, be2) }
;

%inline unop:
  | NOT { Not }
;

%inline binop:
  | PLUS { Plus }
  | FOIS { Times }
  | MOINS { Less }
  | DIV { Div }
;

atom:
  | i = INT { Int i}
;

ident:
  | id = IDENT { id }
;
