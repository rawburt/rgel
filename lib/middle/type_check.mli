val check :
  string ->
  Parsed_ast.parsed_module ->
  (Typed_ast.typed_module, Errors.error list) Result.t
