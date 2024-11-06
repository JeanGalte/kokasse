%{
    open Ast

    exception Empty_elif_list

    exception Impossible
    
    let imbricated_elif (lb : (expr * expr) list) (a : expr) (b : expr) (c : expr) : expr =      
      let rec aux ( l : (expr * expr) list) (acc : expr) = 
	(match l with
	 | [(cond, e)] ->
	    (
	      match acc with
	      | If_then_else (e1, e2, e3) -> If_then_else (e1, e2, (If_then_else (cond, e ,e3)))
	      | _ -> raise Impossible 
	    )
	 | (cond, e) :: tl ->
	    (
	      match acc with
	      | If_then_else (e1, e2, e3) -> aux tl (If_then_else (e1, e2, (If_then_else (cond, e ,e3))))
	      | _ -> raise Impossible
	    )
	 | _ -> raise Empty_elif_list
	)
      in aux lb (If_then_else (a,b,c))
%}

%token EOF

%token LB RB LP RP CRG CRD LAB RAB COMMA DOUBLE_DOT DOUBLE_DOT_EGAL SEMIC

%token PLUS MOINS FOIS DIV MOD GT EGAL
%token AND OR LT

%token NOT NINT

%token FUN FN IF THEN ELSE RETURN ELIF VAL VAR

%token <int> INT
%token <bool> BOOL
%token <string> STRING

%token <string> IDENT

%type <Ast.program> prog

%nonassoc SEMIC

%start prog

%%

//Un programme est une suite de définitions de fonctions nécessairement non vide 
prog:
    p = fun_def+
    EOF
    { p }
;

fun_def:
    FUN name=ident f=fun_body
    { (name,f) }
;

fun_body:
  | LP args_spec=separated_list(COMMA, arg_type_spec) r=ret_type? RP LB e=expr RB
    {{args=args_spec; ret_type=r; body=e}}
;

ret_type:
  | DOUBLE_DOT r=otype { r } 
;

arg_type_spec:
  | i=ident DOUBLE_DOT t=otype {(i, t)}
;

expr:
  | b = bexpr { b }
;

bexpr:
  | e = if_expr { e }
  | e = or_expr { e }
  | LP be=bexpr RP { be }
  | FN f=fun_body {Fn f}
  | RETURN e=expr {Return e}
  | i=ident DOUBLE_DOT_EGAL e=expr { Stmts [Var (i,e)] }
  | b=block {b}
;

list_expr:
  | CRG l = separated_list(COMMA, expr) CRD {Elist l}

block:
  | LB l=block_part* RB {Stmts l}
;

block_part:
  | SEMIC* s=stmt SEMIC+ {s}
;

stmt:
  | e = bexpr {E e}
  | VAL i=ident EGAL e=expr {Val (i, e)}
  | VAR i=ident DOUBLE_DOT_EGAL e=expr {Var (i, e)}
;

if_expr:
  | IF e=bexpr THEN e1=expr ELSE e2=expr {If_then_else (e, e1, e2)}
  | IF e1=bexpr RETURN e2=expr {If_then_else (e1, Return e2, Stmts [])}
  | IF e=bexpr THEN e1=expr l=nonempty_list(elif_expr) ELSE e2=expr {imbricated_elif l e e1 e2}
;

elif_expr:
  | LP ELIF e1=bexpr THEN e2=expr RP {(e1, e2)} 
;

or_expr:
  | e1 = and_expr OR e2 = and_expr { Binop (Or, e1, e2) }
  | e=and_expr {e}
;

and_expr:
  | e1 = bool_expr1 AND e2 = bool_expr1 { Binop (Lt, e1, e2) }
  | e=bool_expr1 {e}
;

bool_expr1:
  | NOT e=bool_expr1 {Unop (Not, e)}
  | e=bool_expr2 { e }
;

bool_expr2:
  | e1 = arith_expr1 LT e2 = arith_expr1 {Binop (Lt, e1, e2)}
  | e1 = arith_expr1 GT e2 = arith_expr1 {Binop (Gt, e1, e2)}
  | e1 = arith_expr1 LAB e2 = arith_expr1 {Binop (Slt, e1, e2)}
  | e1 = arith_expr1 RAB e2 = arith_expr1 {Binop (Sgt, e1, e2)}
  | e = arith_expr1 {e}
;

arith_expr1:
  | e1 = arith_expr1 PLUS e2 = arith_expr2 { Binop (Plus, e1, e2)}
  | e1 = arith_expr1 MOINS e2 = arith_expr2 {Binop (Less, e1, e2 ) }
  | e = arith_expr2 {e}
;

arith_expr2:
  | e1 = arith_expr2 FOIS e2 = arith_expr3 { Binop (Times, e1, e2)}
  | e1 = arith_expr2 DIV e2 = arith_expr3 { Binop (Div, e1, e2)}
  | e1 = arith_expr2 MOD e2 = arith_expr3 { Binop (Mod, e1, e2) }
  | e = arith_expr3 {e}
;

arith_expr3:
  | NINT e=arith_expr3 { Unop (Nint, e) }
  | e = atom_expr {e}
;

atom_expr:
  | b = BOOL { Atom (Bool b) }
  | i = INT { print_newline () ;  Atom (Int i)}
  | s = STRING {Atom (String s)}
  | l=list_expr {Atom l}
;

otype:
  | id=ident LAB t=otype RAB {Composed_type (id, t)}
  | id = ident {Typename id}
  | LP RP { Typename "unit" }
;

ident:
  | id = IDENT { id }
;
