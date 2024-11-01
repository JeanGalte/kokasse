type ident = string

type binop = Plus | Less2 | Times | Div

type unop = Not | Less1

type effect =
  | No
  | Console
  | Div

type o_type =
  | String
  | Int
  | Bool
  | Unit
  | Const of string
  | List_t of o_type list
  | Maybe_t of o_type
  | Calcul_t of (o_type list) * o_effect_type
and
  o_effect_type = o_type * effect

type param = (ident * o_type)

type atom =
  | True
  | False
  | Int of int
  | String of string
  | U of unit
  | Id of ident

type expr =
  | STMTS of stmt list
  | Atom of atom
  | Unop of (unop * expr)
  | Binop of (binop * expr * expr)
  | If_then_else of (expr * expr * expr)
  | Fn of funbody
  | Fcall of (ident * expr list)
  | Return of expr 
and
  stmt =
  | Val of (ident * expr)
  | Var of (ident * expr)
and
  funbody =
  {
    args : param list;
    ret_type : o_type;
    body : expr;    
  }

type decl = ident*funbody 

type program = decl list 
