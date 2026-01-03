let parse_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  Lexing.set_filename lexbuf filename;
  try
    let ast = Rgel.Grammar.toplevels Rgel.Lexer.token lexbuf in
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
  let module_toplevels = parse_file filename in
  Rgel.Parsed_ast.{ module_name; module_toplevels }

let handle_errors () =
  Rgel.Errors.display_error_log ();
  exit 1

let () =
  let config = Config.parse () in
  if config.trace then Rgel.Debug.trace := true;
  let parsed_module = parse_module config.input_file in
  let env = Rgel.Type_loader.load_module config.entry parsed_module in
  if not (Rgel.Errors.is_error_log_empty ()) then handle_errors ()
  else
    let typed_module = Rgel.Type_check.check env parsed_module in
    if not (Rgel.Errors.is_error_log_empty ()) then handle_errors ()
    else
      let output_code =
        Rgel.Codegen.emit config.runtimejs config.entry typed_module
      in
      let oc = open_out config.output_file in
      output_string oc output_code;
      close_out oc;
      flush_all ();
      if config.quickjs then
        let _ = Sys.command (Printf.sprintf "qjs %s" config.output_file) in
        ()
