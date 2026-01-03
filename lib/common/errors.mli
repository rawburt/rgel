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
  | Redeclared_identifier of string
  | Not_a_record of Types.t
  | Record_field_not_found of string * Types.t
  | Record_field_mismatch of {
      record_name : string;
      expected_fields : (string * Types.t) list;
      actual_fields : (string * Types.t) list;
    }
  | Return_outside_function

type error =
  | Syntax_error of syntax_error * Location.t
  | Type_error of type_error * Location.t

exception CompilerError of error

val throw_syntax_error : syntax_error -> Location.t -> 'a
val show_error : error -> string
val log_syntax_error : syntax_error -> Location.t -> unit
val log_type_error : type_error -> Location.t -> unit
val is_error_log_empty : unit -> bool
val display_error_log : unit -> unit
