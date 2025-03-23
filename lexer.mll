{
  open Lexing
  open Parser

  exception SyntaxError of string

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
      lexbuf.lex_curr_p <-
        { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z'] (alpha | '_' | '\'' | digit)*
let integer = digit+

rule token = parse
  | '\n'  { newline lexbuf; token lexbuf }
  | space  { token lexbuf }
  | "(*"  { comment 0 lexbuf }

  | '=' { EQ }

  | '+'  { PLUS }
  | '-'  { MINUS }
  | '*'  { MULT }
  | '/'  { DIV }

  | "+." { FPlus }    (* Added float plus operator *)
  | "-." { FMinus }   (* Added float minus operator *)
  | "*." { FMult }    (* Added float multiplication operator *)
  | "/." { FDiv }     (* Added float division operator *)

  | "true" { TRUE }
  | "false" { FALSE }
  | "&&" { LAND }
  | "||" { LOR }
  | "not" { NOT }
  | ">" { GREAT }
  | ">=" { GREATEQ }
  | "<" { LESS }
  | "<=" { LESSEQ }
  | "<>" { NEQ }
  | "!=" { NEQ }

  | "let"  { LET }
  | "in"  { IN }

  | "print_int" { PRINT_INT }  (* Added print_int token *)
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "float" { TFLOAT }         (* Added float token *)

  | "int" { TINT }
  | "bool" { TBOOL }
  | "unit" { TUNIT }


  | '('  { LPAR }
  | ')'  { RPAR }
  | ','  { COMMA }
  | ':'  { COLON }
  | ';'  { SEMICOLON }  (* Sequencing operator is here *)
  
  | eof  { EOF }

  | float_literal as f { FLOAT (float_of_string f) }  (* Float literal rule; must come before integer *)
  | integer as n  { INT (int_of_string n) }
  | ident as id  { VAR id }

  | _  { raise Error }

and comment depth = parse
  | '\n'  { newline lexbuf; comment depth lexbuf }
  | "(*"  { comment (depth + 1) lexbuf }
  | "*)"
    {
      match depth with
      | 0 -> token lexbuf
      | _ -> comment (depth - 1) lexbuf
    }
  | eof     { raise Error }
  | _       { comment depth lexbuf }
