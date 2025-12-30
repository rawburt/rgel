let usage_msg = "rgel <file>"
let input_files = ref []
let anon_fun filename = input_files := filename :: !input_files
let speclist = []

let parse_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  Lexing.set_filename lexbuf filename;
  try
    let ast = Rgel.Grammar.module_defs Rgel.Lexer.token lexbuf in
    close_in ic;
    ast
  with
  | Rgel.Grammar.Error ->
      let loc =
        Rgel.Location.make_loc
          (lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p)
      in
      Printf.eprintf "%s: parse error at token \"%s\"\n"
        (Rgel.Location.string_of_loc loc)
        (Lexing.lexeme lexbuf);
      close_in ic;
      exit 1
  | Rgel.Errors.CompilerError error ->
      Printf.eprintf "%s\n" (Rgel.Errors.show_error error);
      close_in ic;
      exit 1

let parse_module filename =
  let module_name = Filename.basename filename |> Filename.remove_extension in
  let module_defs = parse_file filename in
  Rgel.Parsed_ast.{ module_name; module_defs }

let () =
  Arg.parse speclist anon_fun usage_msg;
  if List.length !input_files <> 1 then (
    print_endline "No input file provided.";
    exit 1)
  else
    let input_file = List.hd !input_files in
    let parsed_module = parse_module input_file in
    print_endline (Rgel.Parsed_ast.show_parsed_module parsed_module)
