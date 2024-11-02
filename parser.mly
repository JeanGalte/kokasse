%{
    open Ast
 %}

%token EOF

%token LB RB LP RP

%token PRINT

%token FUN

%token <int> INT
%token <bool> BOOL

%token NOT

%token PLUS MOINS FOIS DIV
%token AND OR 

%left PLUS MOINS OR AND
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
  | LP be = bexpr RP { be } 
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
  | AND { And }
  | OR { Or }
;

atom:
  | b = BOOL { Bool b }
  | i = INT { Int i}
;

ident:
  | id = IDENT { id }
;
