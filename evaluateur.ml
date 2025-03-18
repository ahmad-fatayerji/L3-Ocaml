(*
  Auteur : FATAYERJI Ahmad
  Groupe : 684J
*)

open Syntax

(* Type pour représenter les valeurs calculées dans SimpleML *)
type value = VInt of int | VBool of bool

(* Environnement d'évaluation : association d'identifiants à leur valeur *)
type env_val = (string * value) list

(* eval_expr : env_val -> expr -> value*)
(* Évalue une expression dans l'environnement env et renvoie une valeur. *)
let eval_expr (env : env_val) (e : expr) : value =
  (* code ici *)
  VInt 0 (* placeholder *)

(* eval_prog : prog -> value*)
(*Évalue l'ensemble du programme en exécutant la fonction main.*)
let eval_prog (p : programme) : value =
  (* code ici *)
  VInt 0 (* placeholder *)
