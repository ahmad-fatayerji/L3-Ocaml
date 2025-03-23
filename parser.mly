%{
  open Syntax
%}


%token EOF
%token <int> INT
%token <float> FLOAT (* Added float token *)
%token <Syntax.idvar> VAR
%token EQ
%token PLUS MINUS MULT DIV
%token FPLUS FMINUS FMULT FDIV   (* Added float operators *)
%token LAND LOR NOT
%token NEQ GREAT GREATEQ LESS LESSEQ
%token TRUE FALSE
%token LPAR RPAR COMMA COLON 
%token LET IN
%token IF THEN ELSE

%token PRINT_INT                (* Added print_int token *)

%token TINT
%token TBOOL
%token TUNIT
%token TFLOAT                  (* Added float token *)
$%token SEMICOLON              (* Added sequencing operator *)

%left ELSE IN
%nonassoc NOT
%nonassoc EQ NEQ GREAT GREATEQ LESS LESSEQ
%left LOR
%left LAND
%left PLUS MINUS
%left MULT DIV
%left SEMICOLON

%start prog
%type <Syntax.programme> prog


%%

prog: list_implem_decl; EOF  { $1 }

ty:
  | TBOOL        { TBool }
  | TINT         { TInt }
  | TUNIT        { TUnit }

fun_decl:
  | LET VAR LPAR list_typed_ident RPAR COLON ty EQ expr
    { {id = $2 ; var_list = $4 ; typ_retour = $7; corps = $9} }

list_implem_decl:
  |  { [] }
  | list_implem_decl fun_decl {$2::$1}


expr:
  | VAR             { Var $1 }
  | INT             { Int $1 }
  | FLOAT               { Float $1 }   (* Modified: Supports float literals *)
  | TRUE            { Bool true }
  | FALSE           { Bool false }
  | LPAR expr RPAR   { $2 }
  | app_expr { $1 }
  | IF expr THEN expr ELSE expr        { If ($2, $4, $6) }
  | LET LPAR VAR COLON ty RPAR EQ expr IN expr
    { Let ($3, $5, $8, $10) }
  | expr PLUS expr     { BinaryOp (Plus, $1, $3) }
  | expr MINUS expr    { BinaryOp (Minus, $1, $3) }
  | expr MULT expr     { BinaryOp (Mult, $1, $3) }
  | expr DIV expr      { BinaryOp (Div, $1, $3) }
  | expr FPLUS expr     { BinaryOp (FPlus, $1, $3) }   (* Added: float addition *)
  | expr FMINUS expr    { BinaryOp (FMinus, $1, $3) }  (* Added: float subtraction *)
  | expr FMULT expr     { BinaryOp (FMult, $1, $3) }   (* Added: float multiplication *)
  | expr FDIV expr      { BinaryOp (FDiv, $1, $3) }    (* Added: float division *)
  | NOT expr          { UnaryOp (Not, $2) }
  | expr LAND expr     { BinaryOp (And, $1, $3) }
  | expr LOR expr      { BinaryOp (Or, $1, $3) }
  | expr EQ expr      { BinaryOp (Equal, $1, $3) }
  | expr NEQ expr     { BinaryOp (NEqual, $1, $3) }
  | expr GREAT expr    { BinaryOp (Great, $1, $3) }
  | expr GREATEQ expr  { BinaryOp (GreatEq, $1, $3) }
  | expr LESS expr    { BinaryOp (Less, $1, $3) }
  | expr LESSEQ expr  { BinaryOp (LessEq, $1, $3) }
  | expr SEMICOLON expr { Seq ($1, $3) } (* Added sequencing operator *)
  | expr NOT expr       { UnaryOp (Not, $2) }  (* Modified: Adjusted pattern for 'not' *)

app_expr:
  | VAR LPAR list_expr RPAR { App ($1, $3) }

typed_ident:
  | VAR COLON ty { ($1,$3) }

list_expr :
  |  { [] }
  | expr { [$1] }
  | expr COMMA list_expr {$1::$3}

list_typed_ident :
  |  { [] }
  | typed_ident { [$1] }
  | typed_ident COMMA list_typed_ident   {$1::$3}

%%
