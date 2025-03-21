(* Auteurs: 684J
Ahmad Fatayerji
Théo Chouin
*)

open Syntax

type env_type = { l_variables: (idvar * typ) list ; l_functions: fun_decl list }

let rec check_var v a l = match (v,a,l) with
	| (_,_,[]) -> failwith "use of undeclared variable !"
	| (x,y,(a,b)::l') -> if (x = a) then
 						if (y = b)
       						then true
	     					else false
					else check_var x y l'

let rec check_fun f a l = match (f,a,l) with
	| (_,_,[]) -> failwith "use of undeclared function !"
	| (x,y,f'::l') -> if (x = f'.id) then
 						if (y = f'.typ_retour)
       						then true
						else false
					else check_fun x y l'

let rec verif_expr expression type_attendu environment = match (expression,type_attendu,environment) with
	| (Int _,TInt,_) -> true
	| (Bool _,TBool,_) -> true
	| (Var v,attente,env) -> check_var v attente env.l_variables
	| (IdFun f,attente,env) -> check_fun f attente env.l_functions
	| (BinaryOp (x,y,z),attente,env) -> begin match (x,attente) with
 		| (Plus,TInt) 	| (Minus,TInt) | (Mult,TInt) | (Div,TInt) | (Equal,TInt)
   		| (NEqual,TInt) | (Less,TInt) 	| (LessEq,TInt) | (Great,TInt)
	  	| (GreatEq,TInt) -> (verif_expr z TInt env) && (verif_expr y TInt env)
     		| (And,TBool) 	| (Or,TBool) | (Equal,TBool)
		| (NEqual,TBool) -> (verif_expr z TBool env) && (verif_expr y TBool env)
		| _ -> false
		end
	| (UnaryOp (_,x),TBool,env) -> verif_expr x TBool env
	| (If (x,y,z),attente,env) -> (verif_expr x TBool env) && (verif_expr y attente env) && (verif_expr z attente env)
	| (Let (a,b,c,d),attente,env) -> if (verif_expr c b env) then verif_expr d attente {l_variables = (a,b)::env.l_variables ; l_functions = env.l_functions} else false
	(*| (App (a,b),attente,env) -> false*)
	| _ -> false

let verif_decl_fun funct environment = match (funct,environment) with
	| _ -> false

let verif_prog program = match program with
	| _ -> false
