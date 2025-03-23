(* Auteurs: 684J
Ahmad Fatayerji
Théo Chouin
*)

open Syntax

type env_type = { l_variables: (idvar * typ) list ; l_functions: fun_decl list }

(* findVar: Searches for the variable 'x' in the type environment 'environement' 
   (a list of (idvar * typ) pairs). Returns Some varType if found, 
   or None if the variable is not declared. *)
		let rec findVar environement x =
			match environement with
			| [] -> None
			| (varName, varType) :: rest ->
					if varName = x then Some varType
					else findVar rest x

(* findFunction: Searches for the function named 'f' in the program 'program'
   (a list of function declarations). If found, returns Some (params, returnType),
   where 'params' is the list of (idvar * typ) pairs (i.e., the function's parameters)
   and 'returnType' is the declared return type of the function.
   Returns None if no matching function is found. *)
		let rec findFunction program f =
			match program with
			| [] -> None
			| fdecl :: rest ->
					if fdecl.id = f then Some (fdecl.var_list, fdecl.typ_retour)
					else findFunction rest f

(* verif_expr: Verifies that the expression 'expr' has the expected type 'exp_type'
   under the type environment 'environement' (a list of (idvar * typ) pairs) and using the
   program 'program' for function lookups.
   Returns true if the expression is well-typed; false otherwise. *)
		let rec verif_expr environement program expr exp_type =
			match expr with
			| Var x ->
					(match findVar environement x with
					 | Some found_type -> found_type = exp_type
					 | None -> false)  (* Variable not declared *)
			| Int _ -> exp_type = TInt
			| Float _ -> exp_type = TFloat
			| Bool _ -> exp_type = TBool
			| BinaryOp (op, e1, e2) ->
					(match op with
					 | Plus | Minus | Mult | Div ->
							 verif_expr environement program e1 TInt &&
							 verif_expr environement program e2 TInt &&
							 exp_type = TInt
					 | FPlus | FMinus | FMult | FDiv ->
							 verif_expr environement program e1 TFloat &&
							 verif_expr environement program e2 TFloat &&
							 exp_type = TFloat
					 | And | Or ->
							 verif_expr environement program e1 TBool &&
							 verif_expr environement program e2 TBool &&
							 exp_type = TBool
					 | Equal | NEqual | Less | LessEq | Great | GreatEq ->
							 let ints_ok = verif_expr environement program e1 TInt && verif_expr environement program e2 TInt in
							 let bools_ok = verif_expr environement program e1 TBool && verif_expr environement program e2 TBool in
							 ((ints_ok || bools_ok) && exp_type = TBool))
			| UnaryOp (Not, e) ->
					verif_expr environement program e TBool && exp_type = TBool
			| If (cond, e_then, e_else) ->
					verif_expr environement program cond TBool &&
					verif_expr environement program e_then exp_type &&
					verif_expr environement program e_else exp_type
			| Let (x, t, e1, e2) ->
					verif_expr environement program e1 t &&
					verif_expr ((x, t) :: environement) program e2 exp_type
			| App (f, args) ->
					(match findFunction program f with
					 | Some (params, ret_ty) ->
							 List.length params = List.length args &&
							 List.for_all2 (fun (_, param_ty) arg ->
								 verif_expr environement program arg param_ty) params args &&
							 ret_ty = exp_type
					 | None -> false)
			| Seq (e1, e2) ->
					((* e1 can be any allowed type; here we check for int, bool, unit, or float *)
					 (verif_expr environement program e1 TInt ||
						verif_expr environement program e1 TBool ||
						verif_expr environement program e1 TUnit ||
						verif_expr environement program e1 TFloat))
					&&
					verif_expr environement program e2 exp_type
			| PrintInt e ->
					verif_expr environement program e TInt && exp_type = TUnit
		
(* verif_decl_fun: Verifies that the function declaration 'decl' is well-typed.
   It extends the current type environment 'environement' with the parameters of the function,
   then checks that the body (decl.corps) has the declared return type (decl.typ_retour)
   using the verif_expr function.
   Returns true if the function declaration is well-typed; false otherwise. *)
		let verif_decl_fun environement program decl =
			let extended_env = decl.var_list @ environement in
			verif_expr extended_env program decl.corps decl.typ_retour

(* verif_prog: Verifies that the entire program 'program' is well-typed.
   It performs two main checks:
   1. Ensures there is a 'main' function with no parameters and an acceptable return type.
   2. Checks that every function declaration in the program is well-typed using verif_decl_fun.
   Returns true if both checks pass; otherwise, returns false. *)
		let verif_prog program =
			(* Check for a valid main function *)
			let main_exists =
				List.exists (fun decl ->
					decl.id = "main" &&
					decl.var_list = [] &&
					(decl.typ_retour = TUnit ||
					 decl.typ_retour = TBool ||
					 decl.typ_retour = TInt ||
					 decl.typ_retour = TFloat))
				program
			in
			(* Verify all function declarations in the program *)
			main_exists && List.for_all (fun decl -> verif_decl_fun [] program decl) program
		