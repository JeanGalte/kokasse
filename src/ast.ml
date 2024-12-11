type ident = string

type binop =
  | Plus | Less | Times | Div | Mod | Concat | And | Or
  | Lt | Gt | Slt | Sgt | Eq | Neq

type unop = Not | Nint

type effect = ident
  
type o_type =
  | Typename of ident
  | Composed_type of (ident * o_type)
  | Calcul_t of (o_type list) * result_type
and
  result_type = o_type * (effect list)

type param = (ident * o_type)

type expr =
  | Stmts of stmt list
  | Unop of (unop * expr)
  | Binop of (binop * expr * expr)
  | If_then_else of (expr * expr * expr)
  | Fn of funbody
  | Fcall of (expr * expr list)
  | Return of expr
  | Elist of expr list
  | Bool of bool 
  | Int of int
  | String of string
  | Unit
  | Id of ident
and
  stmt =
  | E of expr
  | Val of (ident * expr)
  | Var of (ident * expr)
and
  funbody =
  {
    args : param list;
    ret_type : result_type option;
    body : expr;    
  }
 
type decl = ident*funbody 

type program = decl list 
