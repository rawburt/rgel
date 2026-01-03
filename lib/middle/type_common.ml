open Parsed_ast

let translate_type env parsed_type =
  match parsed_type.type_desc with
  | Type_name name -> (
      match Env.find_type env name with
      | Some t -> t
      | None ->
          Env.error (Errors.Type_not_found name) parsed_type.type_loc;
          Types.fresh_var ())

let ordered_toplevels toplevels =
  let type_decls, defs =
    List.partition
      (function
        | Toplevel_extern _ | Toplevel_rec _ -> true | Toplevel_def _ -> false)
      toplevels
  in
  type_decls @ defs
