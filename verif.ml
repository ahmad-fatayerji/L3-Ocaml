(*
  Auteur : FATAYERJI Ahmad
  Groupe : 684J
*)

open Syntax

(* Environnement de typage : association d'identifiants à leur type SimpleML *)
type env_type = (string * typ) list

(* verif_expr : env_type -> expr -> typ -> bool
Vérifie que l'expression e est de type expected dans l'environnement env.*)
let verif_expr (env : env_type) (e : expr) (expected : typ) : bool =
  (* code ici *)
  true
(* placeholder *)

(* verif_decl_fun : env_type -> decl_fun -> bool
Vérifie qu'une déclaration de fonction est correctement typée dans l'environnement env.*)
let verif_decl_fun (env : env_type) (decl : fun_decl) : bool =
  (* code ici *)
  true
(* placeholder *)

(* verif_prog : prog -> bool
Vérifie que l'ensemble du programme est bien typé, notamment que la fonction main existe
et que toutes les déclarations sont correctes.*)
let verif_prog (p : programme) : bool =
  (* code ici *)
  true

(* version théo

type env_type = { l_variables: (Syntax.idvar * Syntax.typ) list ; l_functions: Syntax.fun_decl list }

let rec check_var v a l = match v,a,l with
	| _,_,[] -> failwith "use of undeclared variable !"
	| x,y,(x,y)::l' -> true
	| x,_,(x,_)::l' -> false
	| x,y,z::l' -> check_var x y l'

let rec check_fun f a l = match f,a,l with
	| _,_,[] -> failwith "use of undeclared function !"
	| x,y,(x,_,y,_)::l' -> true
	| x,_,(x,_,_,_)::l' -> false
	| x,y,z::l' check_fun x y l'

let rec verif_expr expression type_attendu environment = match (expression,type_attendu,environment) with
	| (Int i,TInt,_) -> true
	| (Bool b,TBool,_) -> true
	| (Var v,attente,env) -> check_var v attente env.l_variables
	| (IdFun f,attente,env) -> check_fun f attente env.l_functions
	|
	| _ -> false

let verif_decl_fun funct environment = match (funct,environment) with
	| _ -> false

let verif_prog program = match program with
	| _ -> false
 
*)
