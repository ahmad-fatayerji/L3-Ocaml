(* Auteurs: 684J
Ahmad Fatayerji
Théo Chouin
*)

open Syntax

type env_type = { l_variables: (idvar * typ) list ; l_functions: fun_decl list }

(* on cherche l'idvar passé en argument dans une liste de type (idvar*typ)list, si l'id est trouvé on compare le typ associé avec le type attendu passé en argument
retourne vrai si le type attendu est identique au type de la variable recherchée, retourne faux sinon *)
let rec check_var v att l = match (v,l) with
	| (_,[]) -> failwith "Error check_var : use of undeclared variable !"
	| (x,(a,b)::l') -> if (x = a) then
 						if (att = b)
       						then true
	     					else false
					else check_var x att l'

(* même fonctionnement que la fonction check_var mais avec un idfun et une liste de type fun_decl list
retourne vrai si le type attendu est identique au type de la fonction recherchée, retourne faux sinon *)
let rec check_fun f att l = match (f,l) with
	| (_,[]) -> failwith "Error check_fun : use of undeclared function !"
	| (x,f'::l') -> if (x = f'.id) then
 						if (att = f'.typ_retour)
       						then true
						else false
					else check_fun x att l'

(* retourne la liste des arguments de la fonction dont l'identifiant est entrée en paramètre *)
let rec get_decl_arg id fList = match fList with
	| (a,b,_,_)::fL' -> if ( a = id ) then b else get_decl_arg id fL'
 	| [] -> []

(* fonction vérifiant si le type attendu est le même que le type de l'expression passé en argument *)
let rec verif_expr expression type_attendu environment = 
	(* fonction auxiliaire servant si l'expression est App, vérifie si la liste d'argument donnée en entrée de la fonction correspond à la liste d'argument dans la déclaration de la fonction *)
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

(* TO DO *)
let verif_decl_fun f env = match (f,env) with
	| _ -> false

(* vérification du typage d'une liste de déclaration de fonction *)
let verif_prog program =
	let rec verif_aux l_fun env = match l_fun with
 		| x::l_fun' -> if verif decl_fun funct env then verif_aux l_fun' { l_variables = env.l_variables ; l_functions = x::env.l_functions } else false
   		| [] -> true
  	in
   verif_aux program { l_variables = [] ; l_functions = [] }
