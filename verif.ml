(* Auteurs: 684J
Ahmad Fatayerji
Théo Chouin
*)

open Syntax

type env_type = { l_variables: (idvar * typ) list ; l_functions: fun_decl list }

let rec check_var v a l = match (v,a,l) with
	| (_,_,[]) -> failwith "Error check_var : use of undeclared variable !"
	| (x,y,(a,b)::l') -> if (x = a) then
 						if (y = b)
       						then true
	     					else false
					else check_var x y l'

let rec check_fun f a l = match (f,a,l) with
	| (_,_,[]) -> failwith "Error check_fun : use of undeclared function !"
	| (x,y,f'::l') -> if (x = f'.id) then
 						if (y = f'.typ_retour)
       						then true
						else false
					else check_fun x y l'

let rec get_decl_arg id fList = match fList with
	| (a,b,_,_)::fL' -> if ( a = id ) then b else get_decl_arg id fL'
 	| [] -> []

let rec verif_expr expression type_attendu environment = 
	let rec check_arg decl arg env = match (decl,arg) with
	| ( (_,a)::decl' , y::arg' ) -> if verif_expr y a env then check_arg decl' arg' env else false
 	| ( _::_ , [] ) -> failwith "Error check_arg : not enough arguments !"
  	| ( [] , _::_ ) -> failwith "Error check_arg : too many arguments !"
   	| ( [] , [] ) -> true
	in
	match (expression,type_attendu,environment) with
	| (Int _,TInt,_) -> true
	| (Bool _,TBool,_) -> true
	| (Var v,attente,env) -> check_var v attente env.l_variables
	| (IdFun f,attente,env) -> check_fun f attente env.l_functions
	| (BinaryOp (x,y,z),attente,env) -> begin match (x,attente) with
 		| (Plus,TInt) 	| (Minus,TInt) | (Mult,TInt) | (Div,TInt)
	  	| (GreatEq,TInt) -> (verif_expr z TInt env) && (verif_expr y TInt env)
     		| (And,TBool) 	| (Or,TBool) -> (verif_expr z TBool env) && (verif_expr y TBool env)
		| (Equal,TBool) | (NEqual,TBool) -> ( (verif_expr z TBool env) && (verif_expr y TBool env) ) || ( (verif_expr z TInt env) && (verif_expr y TInt env) )
  		| (Less,TBool) | (LessEq,TBool) | (Great,TBool) -> (verif_expr z TInt env) && (verif_expr y TInt env)
		| _ -> false
		end
	| (UnaryOp (_,x),TBool,env) -> verif_expr x TBool env
	| (If (x,y,z),attente,env) -> (verif_expr x TBool env) && (verif_expr y attente env) && (verif_expr z attente env)
	| (Let (a,b,c,d),attente,env) -> if (verif_expr c b env) then verif_expr d attente {l_variables = (a,b)::env.l_variables ; l_functions = env.l_functions} else false
	| (App (a,b),attente,env) -> if check_fun a attente env.l_functions then check_arg ( get_decl_arg a env.l_functions ) b env else false
	| _ -> false

let verif_decl_fun f env = match (f,env) with
	| _ -> false

let verif_prog program =
	let rec verif_aux l_fun env = match l_fun with
 		| x::l_fun' -> if verif decl_fun funct env then verif_aux l_fun' { l_variables = env.l_variables ; l_functions = x::env.l_functions } else false
   		| [] -> true
  	in
   verif_aux program { l_variables = [] ; l_functions = [] }
