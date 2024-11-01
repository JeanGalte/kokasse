type ident = string

type binop = Plus | Less2 | Times | Div

type unop = Not | Less1

type effect =
  | No
  | Console
  | Div

type o_type =
  | String of string
  | Int of int
  | Bool of bool
  | Unit
  | Const of string
  | List_t of o_type list
  | Maybe_t of o_type
  | Calcul_t of (o_type list) * o_effect_type
and
  o_effect_type = o_type * effect

type param = (ident * o_type) list 

type expr = STMTS of stmt list | Bexpr 
and
  stmt =
  | Bexpr of bexpr
  | Val of (ident * expr)
  | Var of (ident * expr)
and
  bexpr =
  | Atom of atom
  | Not of bexpr
  | Less of bexpr
  | Unop of (unop * bexpr)
  | Binop of (binop * bexpr * bexpr)
  | If_then_else of (bexpr * expr * expr)
  | Fn of funbody
  | Return of expr
and
  funbody =
  {
    args : param list;
    ret_type : o_type;
    body : expr;    
  }
and
  atom =
  | True
  | False
  | Int of int
  | String of string
  | U of unit
  | Id of ident
  | Ex of expr
  | App_l of expr list
  | App of (atom * funbody)
  | Appblock of (atom* (stmt list))
  | Elist of stmt list

type decl = ident -> funbody 

type program = decl list 
