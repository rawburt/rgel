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

let throw_syntax_error syntax_error loc =
  raise (CompilerError (Syntax_error (syntax_error, loc)))

let error_prefix loc = Location.show loc ^ ":"

let show_syntax_error = function
  | Unexpected_token token -> Printf.sprintf "Unexpected token: %s" token
  | String_not_terminated -> "String literal not terminated"
  | Unexpected_string_item -> "Unexpected item in string literal"

let show_type_error = function
  | Entry_not_found name -> Printf.sprintf "Entry not found: %s" name
  | Type_not_found type_name -> Printf.sprintf "Type not found: %s" type_name
  | Identifier_not_found id_name ->
      Printf.sprintf "Identifier not found: %s" id_name
  | Not_a_function t -> Printf.sprintf "Not a function: %s" (Types.show t)
  | Type_mismatch (expected, actual) ->
      Printf.sprintf "Type mismatch: expected %s but got %s"
        (Types.show expected) (Types.show actual)
  | Redeclared_identifier id_name ->
      Printf.sprintf "Redeclared identifier: %s" id_name
  | Not_a_record t -> Printf.sprintf "Not a record: %s" (Types.show t)
  | Record_field_not_found (field_name, t) ->
      Printf.sprintf "Record field not found: %s in %s" field_name
        (Types.show t)
  | Record_field_mismatch { record_name; expected_fields; actual_fields } ->
      let show_fields fields =
        String.concat ", "
          (List.map
             (fun (name, t) -> Printf.sprintf "%s: %s" name (Types.show t))
             fields)
      in
      Printf.sprintf "Record field mismatch: expected %s(%s) but got %s(%s)"
        record_name
        (show_fields expected_fields)
        record_name
        (show_fields actual_fields)
  | Return_outside_function -> "Return statement used outside of a function"

let show_error = function
  | Syntax_error (syntax_error, loc) ->
      Printf.sprintf "%s %s" (error_prefix loc) (show_syntax_error syntax_error)
  | Type_error (type_error, loc) ->
      Printf.sprintf "%s %s" (error_prefix loc) (show_type_error type_error)
