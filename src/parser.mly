%{
    open Ast

    exception Effet_non_reconnu of string

    exception Bloc_ne_se_terminant_pas_par_une_expression
    
    let parse_effect (e : string) : effect =
      if e = "div" then Diverge else
	if e = "console" then Console else
	  raise (Effet_non_reconnu e)

    let rec is_last_expr (sl : stmt list) : bool =
      match sl with
      | [x] -> (match x with |  E _ -> true | _ -> false)
      | _ :: tl -> is_last_expr tl
      | [] -> true

    let check_block (b : stmt list) : unit =
      if (is_last_expr b) then () else raise Bloc_ne_se_terminant_pas_par_une_expression
	  
%}

%token EOF

%token LB RB LP RP CRG CRD LAB RAB COMMA DOUBLE_DOT DOUBLE_DOT_EGAL SEMIC RIGHTARR DOT RET

%token PLUS MOINS FOIS DIV MOD CONCAT GT EGAL DOUBLE_EGAL NEQ
%token AND OR LT

%token NOT NINT

%token FUN FN IF THEN ELSE RETURN VAL VAR

%token <int> INT
%token <bool> BOOL
%token <string> STRING

%token <string> IDENT

%type <Ast.program> prog

%start prog

%%

//Un programme est une suite de définitions de fonctions nécessairement non vide
prog:
    SEMIC* p=fun_def*
    EOF
    { p }
;

fun_def:
    FUN name=ident f=fun_body SEMIC*
    { (name,f) }
;

fun_body:
  | LP args_spec=separated_list(COMMA, arg_type_spec) r=ret_type? RP e=expr
    {{args=args_spec; ret_type=r; body=e}}
;

ret_type:
  | DOUBLE_DOT  r=result { r }
;

result:
  | ef_list=effects_spec? r=otype {(r, ef_list)}
;

effects_spec:
  | LAB s=separated_list(COMMA, ident) RAB {List.map parse_effect s}
;

arg_type_spec:
  | i=ident DOUBLE_DOT t=otype {(i, t)}
;

expr:
  | b = bexpr { b }
  | b = block { b }
;

block:
  | LB SEMIC* sl=stmt_wrap*  RB
    {
      check_block sl;
      Stmts sl
    }
;

stmt_wrap:
  | s=stmt SEMIC+ { s }
;

stmt:
  | e = expr {E e}
  | VAL i=ident EGAL e=expr {Val (i, e)}
  | VAR i=ident DOUBLE_DOT_EGAL e=expr {Var (i, e)}
;

bexpr:
  | e = if_expr { e }
  | e = or_expr { e }
  | FN f=fun_body {Fn f}
  | RETURN e=expr {Return e}
;

if_expr:
  | IF e=bexpr THEN e1=expr ELSE e2=expr {If_then_else (e, e1, e2)}
  | IF e1=bexpr RETURN e2=expr {If_then_else (e1, Return e2, Stmts [])}
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
  | NOT e=bool_expr1 { Unop (Not, e) }
  | e=bool_expr2 { e }
;

bool_expr2:
  | e1 = arith_expr1 LT e2 = arith_expr1 {Binop (Lt, e1, e2)}
  | e1 = arith_expr1 GT e2 = arith_expr1 {Binop (Gt, e1, e2)}
  | e1 = arith_expr1 LAB e2 = arith_expr1 {Binop (Slt, e1, e2)}
  | e1 = arith_expr1 RAB e2 = arith_expr1 {Binop (Sgt, e1, e2)}
  | e1 = arith_expr1 DOUBLE_EGAL e2 = arith_expr1 {Binop (Eq, e1, e2)}
  | e1 = arith_expr1 NEQ e2 = arith_expr1 {Binop (Neq, e1, e2)}
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
  | e1 = arith_expr2 CONCAT e2 = arith_expr3 { Binop (Concat, e1, e2) }
  | e = arith_expr3 {e}
;

arith_expr3:
  | NINT e=arith_expr3 { Unop (Nint, e) }
  | a = atom { a }
;

atom:
  | b = BOOL { Bool b }
  | i = INT {  Int i }
  | s = STRING { String s }
  | i = ident {Id i}
  | LP RP  { Unit }
  | LP e=expr RP {Ex e}
  | a=atom LP l=separated_list(COMMA,expr) RP {Fcall (a, l) }
  | CRG l=separated_list(COMMA, expr) CRD {Elist l}
  | a=atom DOT LP id=ident RP { Fcall (Id id, [a]) }
   /* sucre syntaxique qui lève des conflits  */
  /* | e=expr LP l=separated_list(COMMA, expr) RP FN f=fun_body {Fcall (e, l @ [Fn f])} */
  /* | e=expr b=block { Fcall (e,  [Fn { args = []; ret_type = None; body=b}])} */
;

otype:
  | a = atype { a }
  | a = atype RIGHTARR r=result { Calcul_t ([a], r) }
  | LP t1=otype COMMA t2=otype COMMA tl=separated_list(COMMA, otype) RP RIGHTARR r=result { Calcul_t (t1 :: t2 :: tl, r) }
;

atype:
  | LP RP { Typename "unit" }
  | LP t=otype RP { t }
  | id=ident t=composed_type_arg? {match t with | None -> Typename id | Some x -> Composed_type (id, x)}
;

composed_type_arg:
  | LAB t=otype RAB {t}
;

ident:
  | id = IDENT { id }
;
