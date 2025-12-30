type syntax_error =
  | Unexpected_token of string
  | String_not_terminated
  | Unexpected_string_item

type type_error =
  | Entry_not_found of string
  | Type_not_found of string
  | Identifier_not_found of string
  | Not_a_function of Types.t
  | Type_mismatch of Types.t * Types.t

type error =
  | Syntax_error of syntax_error * Location.t
  | Type_error of type_error * Location.t

exception CompilerError of error

let throw_syntax_error syntax_error loc =
  raise (CompilerError (Syntax_error (syntax_error, loc)))

let error_prefix loc = Location.show loc ^ ":"

let show_syntax_error = function
  | Unexpected_token token -> Printf.sprintf "Unexpected token: %s" token
  | String_not_terminated -> "String literal not terminated"
  | Unexpected_string_item -> "Unexpected item in string literal"

let show_error = function
  | Syntax_error (syntax_error, loc) ->
      Printf.sprintf "%s %s" (error_prefix loc) (show_syntax_error syntax_error)
  | Type_error (type_error, loc) ->
      let type_error_msg =
        match type_error with
        | Entry_not_found name ->
            Printf.sprintf "Entry function not found: %s" name
        | Type_not_found name -> Printf.sprintf "Type not found: %s" name
        | Identifier_not_found name ->
            Printf.sprintf "Identifier not found: %s" name
        | Not_a_function t ->
            Printf.sprintf "Expected a function type, but got: %s"
              (Types.show t)
        | Type_mismatch (t1, t2) ->
            Printf.sprintf "Type mismatch: %s vs %s" (Types.show t1)
              (Types.show t2)
      in
      Printf.sprintf "%s %s" (error_prefix loc) type_error_msg
