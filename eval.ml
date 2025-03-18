(* La fonction suivante récupère le nom de fichier passé en argument de la ligne de commande,
ouvre le fichier et applique l'analyse lexicale dessus, renvoyant une valeur de type Lexing.lexbuf *)
let get_lineBuffer () =
  if Array.length Sys.argv < 2 then
    failwith "Le nom de fichier à évaluer n'a pas été passé en argument."
  else
    try
      let filename = Sys.argv.(1) in
      let inBuffer = open_in filename in
      Lexing.from_channel inBuffer
    with
    | Sys_error msg -> failwith ("Erreur : " ^ msg)
    | Lexer.SyntaxError msg ->
        failwith ("Erreur de parsing dans le programme fourni en entrée :" ^ msg)
    | Parser.Error ->
        failwith "Erreur de syntaxe dans le programme fourni en entrée."

let () =
  let lineBuffer = get_lineBuffer () in
  try
    let prog = Parser.prog Lexer.token lineBuffer in
    (*print_string (Syntax.string_of_programme prog);*)
    let type_ok = Verif.verif_prog prog in
    if type_ok then Evaluateur.eval_prog prog
    else failwith "Erreur de typage dans le programme fourni en entrée."
  with Parser.Error ->
    failwith
      ("Erreur de syntaxe dans le programme fourni en entrée à la position "
      ^ string_of_int (Lexing.lexeme_start lineBuffer))
