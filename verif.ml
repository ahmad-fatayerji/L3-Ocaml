(* Auteurs: 684J
Ahmad Fatayerji
Théo Chouin
*)

open Syntax

type env_type = { l_variables: (idvar * typ) list ; l_functions: fun_decl list }

(* findVar: Searches for the variable 'x' in the type environment 'env' 
   (a list of (idvar * typ) pairs). Returns Some varType if found, 
   or None if the variable is not declared. *)
		let rec findVar env x =
			match env with
			| [] -> None
			| (varName, varType) :: rest ->
					if varName = x then Some varType
					else findVar rest x

(* findFunction: Searches for the function named 'f' in the program 'prog'
   (a list of function declarations). If found, returns Some (params, returnType),
   where 'params' is the list of (idvar * typ) pairs (i.e., the function's parameters)
   and 'returnType' is the declared return type of the function.
   Returns None if no matching function is found. *)
		let rec findFunction prog f =
			match prog with
			| [] -> None
			| fdecl :: rest ->
					if fdecl.id = f then Some (fdecl.var_list, fdecl.typ_retour)
					else findFunction rest f

(* verif_expr: Verifies that the expression 'expr' has the expected type 'expected_ty'
   under the type environment 'env' (a list of (idvar * typ) pairs) and using the
   program 'prog' for function lookups.
   Returns true if the expression is well-typed; false otherwise. *)
		let rec verif_expr env prog expr expected_ty =
			match expr with
			| Var x ->
					(match findVar env x with
					 | Some found_ty -> found_ty = expected_ty
					 | None -> false)  (* Variable not declared *)
			| Int _ -> expected_ty = TInt
			| Float _ -> expected_ty = TFloat
			| Bool _ -> expected_ty = TBool
			| BinaryOp (op, e1, e2) ->
					(match op with
					 | Plus | Minus | Mult | Div ->
							 verif_expr env prog e1 TInt &&
							 verif_expr env prog e2 TInt &&
							 expected_ty = TInt
					 | FPlus | FMinus | FMult | FDiv ->
							 verif_expr env prog e1 TFloat &&
							 verif_expr env prog e2 TFloat &&
							 expected_ty = TFloat
					 | And | Or ->
							 verif_expr env prog e1 TBool &&
							 verif_expr env prog e2 TBool &&
							 expected_ty = TBool
					 | Equal | NEqual | Less | LessEq | Great | GreatEq ->
							 let ints_ok = verif_expr env prog e1 TInt && verif_expr env prog e2 TInt in
							 let bools_ok = verif_expr env prog e1 TBool && verif_expr env prog e2 TBool in
							 ((ints_ok || bools_ok) && expected_ty = TBool))
			| UnaryOp (Not, e) ->
					verif_expr env prog e TBool && expected_ty = TBool
			| If (cond, e_then, e_else) ->
					verif_expr env prog cond TBool &&
					verif_expr env prog e_then expected_ty &&
					verif_expr env prog e_else expected_ty
			| Let (x, t, e1, e2) ->
					verif_expr env prog e1 t &&
					verif_expr ((x, t) :: env) prog e2 expected_ty
			| App (f, args) ->
					(match findFunction prog f with
					 | Some (params, ret_ty) ->
							 List.length params = List.length args &&
							 List.for_all2 (fun (_, param_ty) arg ->
								 verif_expr env prog arg param_ty) params args &&
							 ret_ty = expected_ty
					 | None -> false)
			| Seq (e1, e2) ->
					((* e1 can be any allowed type; here we check for int, bool, unit, or float *)
					 (verif_expr env prog e1 TInt ||
						verif_expr env prog e1 TBool ||
						verif_expr env prog e1 TUnit ||
						verif_expr env prog e1 TFloat))
					&&
					verif_expr env prog e2 expected_ty
			| IdFun _ -> false  (* A function identifier alone is not a valid expression *)
			| PInt e ->
					verif_expr env prog e TInt && expected_ty = TUnit
		
(* verif_decl_fun: Verifies that the function declaration 'decl' is well-typed.
   It extends the current type environment 'env' with the parameters of the function,
   then checks that the body (decl.corps) has the declared return type (decl.typ_retour)
   using the verif_expr function.
   Returns true if the function declaration is well-typed; false otherwise. *)
		let verif_decl_fun env prog decl =
			let extended_env = decl.var_list @ env in
			verif_expr extended_env prog decl.corps decl.typ_retour

(* verif_prog: Verifies that the entire program 'prog' is well-typed.
   It performs two main checks:
   1. Ensures there is a 'main' function with no parameters and an acceptable return type.
   2. Checks that every function declaration in the program is well-typed using verif_decl_fun.
   Returns true if both checks pass; otherwise, returns false. *)
		let verif_prog prog =
			(* Check for a valid main function *)
			let main_exists =
				List.exists (fun decl ->
					decl.id = "main" &&
					decl.var_list = [] &&
					(decl.typ_retour = TUnit ||
					 decl.typ_retour = TBool ||
					 decl.typ_retour = TInt ||
					 decl.typ_retour = TFloat))
				prog
			in
			(* Verify all function declarations in the program *)
			main_exists && List.for_all (fun decl -> verif_decl_fun [] prog decl) prog
		