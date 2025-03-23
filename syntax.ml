(* Auteurs: 684J
Ahmad Fatayerji
Théo Chouin
*)

(* typ represents the types of SimpleML *)
type typ = 
  | TInt 
  | TBool 
  | TUnit  (* Added for the unit type *)
  | TFloat (* Added for the float type *)

(* value represents the possible runtime values of SimpleML *)
type value = 
  | VInt of int 
  | VBool of bool 
  | VFloat of float 
  | VUnit

(* Environments (for type-checking and evaluation) *)
type env_type = (string * typ) list
type env_val = (string * value) list

(* Identifier types *)
type idvar = string
type idfun = string

(* Pour factoriser la présentation des opérateurs binaires, on utilise un type énuméré
binary_op de tous les opérateurs binaires de la syntaxe de SimpleML *)

type binary_op =
  | Plus
  | Minus
  | Mult
  | Div
  | FPlus   (* Added for the float operators *)
  | FMinus  (* Added for the float operators *)
  | FMult   (* Added for the float operators *)
  | FDiv    (* Added for the float operators *)
  | And
  | Or
  | Equal
  | NEqual
  | Less
  | LessEq
  | Great
  | GreatEq

type unary_op = Not

type expr =
  | Var of idvar
  | Int of int
  | Bool of bool
  | BinaryOp of binary_op * expr * expr
  | UnaryOp of unary_op * expr
  | If of expr * expr * expr
  | Let of idvar * typ * expr * expr
  | App of idfun * expr list
  | Seq of expr * expr   (* For M; N *)
  | PInt of expr         (* For print_int *)


(* Définition du type des déclarations de fonction de SimpleML *)
type fun_decl = {
  id: idfun;
  var_list: (idvar * typ) list;
  typ_retour: typ;
  corps: expr;
}

(* A program is a list of function declarations *)
type programme = fun_decl list

(* Display functions for debugging and error messages *)

let string_of_type typ = 
  match typ with 
  | TInt -> "int" 
  | TBool -> "bool" 
  | TUnit -> "unit"   (* Added for the unit type *)
  | TFloat -> "float" (* Added for the float type *)

let string_of_binary_op binop =
  match binop with
  | Plus -> "+"
  | Minus -> "-"
  | Mult -> "*"
  | Div -> "/"
  | FPlus -> "+."   (* Added for the float operators *)
  | FMinus -> "-."  (* Added for the float operators *)
  | FMult -> "*."   (* Added for the float operators *)
  | FDiv -> "/."    (* Added for the float operators *)
  | And -> "and"
  | Or -> "or"
  | Equal -> "="
  | NEqual -> "!="
  | Less -> "<"
  | LessEq -> "<="
  | Great -> ">"
  | GreatEq -> ">="

  let string_of_unary_op unop = 
    match unop with 
    | Not -> "not"

let rec string_of_expr_list expr_list =
  match expr_list with
  | [] -> ""
  | [ e ] -> string_of_expr e
  | e :: expr_list' -> string_of_expr e ^ "," ^ string_of_expr_list expr_list'

  and string_of_expr expr =
    match expr with
    | Var x -> x
    (* Removed branch for IdFun because no such constructor exists in expr *)
    | Int n -> string_of_int n
    | Bool b -> string_of_bool b
    | BinaryOp (binop, expr1, expr2) ->
        "(" ^ string_of_expr expr1 ^ " " ^ string_of_binary_op binop ^ " " ^ string_of_expr expr2 ^ ")"
    | UnaryOp (unop, expr) -> string_of_unary_op unop ^ " " ^ string_of_expr expr
    | If (expr1, expr2, expr3) ->
        "if " ^ string_of_expr expr1 ^ " then " ^ string_of_expr expr2 ^ " else " ^ string_of_expr expr3
    | Let (idvar, typ, expr1, expr2) ->
        "let " ^ idvar ^ " : " ^ string_of_type typ ^ " = " ^ string_of_expr expr1 ^ " in " ^ string_of_expr expr2
    | App (idfun, expr_list) -> idfun ^ "(" ^ string_of_expr_list expr_list ^ ")"
    | Seq (expr1, expr2) -> string_of_expr expr1 ^ "; " ^ string_of_expr expr2  (* For M; N *)
    | PInt expr -> "print_int(" ^ string_of_expr expr ^ ")"                     (* For print_int *)

let rec string_of_var_list var_list =
  match var_list with
  | [] -> ""
  | [ (x, ty) ] -> x ^ ":" ^ string_of_type ty
  | (x, ty) :: var_list' ->
      x ^ ":" ^ string_of_type ty ^ "," ^ string_of_var_list var_list'

let string_of_fun_decl fdecl =
  "let " ^ fdecl.id ^ "(" ^ string_of_var_list fdecl.var_list ^ ") : " ^
  string_of_type fdecl.typ_retour ^ " = " ^ string_of_expr fdecl.corps

  let string_of_programme prog =
  String.concat "\n" (List.map string_of_fun_decl prog)