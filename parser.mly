%{
    open Ast
%}

%token EOF

%token LB RB LP RP

%token PRINT

%token FUN

%token <int> INT

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
    a = atom { Atom a }
;

atom:
    i = INT { Int i }    
;

ident:
    id = IDENT { id }
;
