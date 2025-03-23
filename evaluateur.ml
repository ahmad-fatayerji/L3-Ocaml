(* Auteurs:
Ahmad Fatayerji
Théo Chouin
Groupe 684J
*)

open Syntax

type t_val = V_int of int | V_bool of bool

type env_val = { l_variables: (idvar * t_val) list ; l_functions: (fun_decl * t_val) list }

let rec get_var v varList = match varList with
	| (x,y)::list' -> if (x = v) then y else get_var v list'
 	| [] -> failwith "Error get_var : no value in variable"

let eval_expr expression environment = match (expression,environment) with
	| (Var v,env) -> get_var v env.l_variables
 	| (Int i,env) -> i
  	| (Bool b,env) -> b
   (*
   	| (BinaryOp...
    *
    	| (UnaryOp...
     *
     	| (If...
      *
      	| (Let...
       *
       	| (App...
	*)
 | _ -> failwith "Default error"

let eval_prog program = match program with
	| _ -> ()
