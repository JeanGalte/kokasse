%{
    open Ast
 %}

%token EOF

%token LB RB LP RP COMMA DOUBLE_DOT

%token PRINT

%token FUN

%token <int> INT
%token <bool> BOOL

%token STRING_TYPE BOOL_TYPE INT_TYPE

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
    FUN name=ident LP args_spec=separated_list(COMMA, arg_type_spec)  RP LB e=expr RB
    { (name, { args = args_spec; ret_type = None; body=e}) }
;

arg_type_spec:
    i=ident DOUBLE_DOT t=otype {(i, t)}

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

otype:
  | STRING_TYPE {STRING}
  | INT_TYPE {INT}
  | BOOL_TYPE {BOOL}
;
ident:
  | id = IDENT { id }
;
