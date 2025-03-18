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
