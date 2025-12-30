type syntax_error =
  | Unexpected_token of string
  | String_not_terminated
  | Unexpected_string_item

type type_error = Type_mismatch
type error = Syntax_error of syntax_error * Location.t

exception CompilerError of error

let throw_syntax_error syntax_error loc =
  raise (CompilerError (Syntax_error (syntax_error, loc)))

let error_prefix loc = Location.show loc ^ ": "

let show_syntax_error = function
  | Unexpected_token token -> Printf.sprintf "Unexpected token: %s" token
  | String_not_terminated -> "String literal not terminated"
  | Unexpected_string_item -> "Unexpected item in string literal"

let show_error = function
  | Syntax_error (syntax_error, loc) ->
      Printf.sprintf "%s: %s" (error_prefix loc)
        (show_syntax_error syntax_error)
