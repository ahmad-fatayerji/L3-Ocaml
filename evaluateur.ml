(* Auteurs:
Ahmad Fatayerji
Théo Chouin
Groupe 684J
*)

open Syntax

type t_val = V_int of int | V_bool of bool

type env_val = { l_variables: (idvar * t_val) list ; l_functions: (fun_decl * t_val) list }

let eval_expr expression environment = match (expression,environment) with
	| _ -> ()

let eval_prog program = match program with
	| _ -> ()
