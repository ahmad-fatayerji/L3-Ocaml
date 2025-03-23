(* Auteurs:
Ahmad Fatayerji
Théo Chouin
Groupe 684J
*)

open Syntax

type t_val =
  | VInt of int
  | VFloat of float
  | VBool of bool
  | VUnit

	
type env_val = { l_variables: (idvar * t_val) list ; l_functions: (fun_decl * t_val) list }


(* findVal: Searches for the variable 'x' in the evaluation environment 'env'
   (a list of (idvar * t_val) pairs). Returns its associated value if found,
   or raises an error if not found. *)
		let rec findVal env x =
			match env with
			| [] -> failwith ("Variable " ^ x ^ " not found")
			| (varName, value) :: rest ->
					if varName = x then value
					else findVal rest x
		
(* findFun: Searches for the function named 'f' in the program 'prog'
   (a list of function declarations). Returns the matching function declaration if found,
   or raises an error if not found. *)
		let rec findFun prog f =
			match prog with
			| [] -> failwith ("Function " ^ f ^ " not found")
			| fdecl :: rest ->
					if fdecl.id = f then fdecl
					else findFun rest f

(* eval_expr: Evaluates the expression 'expr' in the environment 'env'
   and using the program 'prog' for function lookups.
   Returns the computed value (of type t_val). *)
	 let rec eval_expr env prog expr =
		match expr with
		| Var x ->
				(* Look up variable 'x' in the environment *)
				findVal env x
		| Int n ->
				VInt n
		| Float f ->
				VFloat f
		| Bool b ->
				VBool b
		| BinaryOp (op, e1, e2) ->
				let v1 = eval_expr env prog e1 in
				let v2 = eval_expr env prog e2 in
				(match (op, v1, v2) with
				 | (Plus, VInt n1, VInt n2) -> VInt (n1 + n2)
				 | (Minus, VInt n1, VInt n2) -> VInt (n1 - n2)
				 | (Mult, VInt n1, VInt n2) -> VInt (n1 * n2)
				 | (Div, VInt n1, VInt n2) ->
						 if n2 = 0 then failwith "Division by zero" else VInt (n1 / n2)
				 | (FPlus, VFloat n1, VFloat n2) -> VFloat (n1 +. n2)
				 | (FMinus, VFloat n1, VFloat n2) -> VFloat (n1 -. n2)
				 | (FMult, VFloat n1, VFloat n2) -> VFloat (n1 *. n2)
				 | (FDiv, VFloat n1, VFloat n2) ->
						 if n2 = 0. then failwith "Division by zero" else VFloat (n1 /. n2)
				 | (And, VBool b1, VBool b2) -> VBool (b1 && b2)
				 | (Or, VBool b1, VBool b2) -> VBool (b1 || b2)
				 | (Equal, VInt n1, VInt n2) -> VBool (n1 = n2)
				 | (Equal, VBool b1, VBool b2) -> VBool (b1 = b2)
				 | (NEqual, VInt n1, VInt n2) -> VBool (n1 <> n2)
				 | (NEqual, VBool b1, VBool b2) -> VBool (b1 <> b2)
				 | (Less, VInt n1, VInt n2) -> VBool (n1 < n2)
				 | (LessEq, VInt n1, VInt n2) -> VBool (n1 <= n2)
				 | (Great, VInt n1, VInt n2) -> VBool (n1 > n2)
				 | (GreatEq, VInt n1, VInt n2) -> VBool (n1 >= n2)
				 | _ -> failwith "Type error in binary operation")
		| UnaryOp (Not, e) ->
				(match eval_expr env prog e with
				 | VBool b -> VBool (not b)
				 | _ -> failwith "Type error in unary operation 'not'")
		| If (cond, e_then, e_else) ->
				(match eval_expr env prog cond with
				 | VBool true -> eval_expr env prog e_then
				 | VBool false -> eval_expr env prog e_else
				 | _ -> failwith "Condition must be a boolean")
		| Let (x, _, e1, e2) ->
				let v1 = eval_expr env prog e1 in
				eval_expr ((x, v1) :: env) prog e2
		| App (f, args) ->
				let fdecl = findFun prog f in
				let arg_vals = List.map (eval_expr env prog) args in
				(* Create a new environment binding function parameters to argument values *)
				let new_env = List.combine (List.map fst fdecl.var_list) arg_vals in
				eval_expr new_env prog fdecl.corps
		| Seq (e1, e2) ->
				let _ = eval_expr env prog e1 in
				eval_expr env prog e2
		| PInt e ->
				(match eval_expr env prog e with
				 | VInt n -> print_int n; print_newline (); VUnit
				 | _ -> failwith "print_int must take an integer")
	

(* eval_prog: Evaluates the program 'prog' by searching for the 'main' function,
   ensuring it takes no arguments, and then evaluating its body.
   The result is printed according to its type. *)
	 let eval_prog prog =
		let main_decl = findFun prog "main" in
		if main_decl.var_list <> [] then failwith "main must take no arguments";
		let result = eval_expr [] prog main_decl.corps in
		match result with
		| VInt n -> print_int n; print_newline ()
		| VFloat f -> print_float f; print_newline ()
		| VBool b -> print_string (string_of_bool b); print_newline ()
		| VUnit -> print_string "- : unit = ()"; print_newline ()
	