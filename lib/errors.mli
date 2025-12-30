type syntax_error =
  | Unexpected_token of string
  | String_not_terminated
  | Unexpected_string_item

type type_error = Type_mismatch
type error = Syntax_error of syntax_error * Location.t

exception CompilerError of error

val throw_syntax_error : syntax_error -> Location.t -> 'a
val show_error : error -> string
