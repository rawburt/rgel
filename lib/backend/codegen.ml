open Typed_ast
module StringMap = Map.Make (String)

let emit_literal = function
  | Parsed_ast.Lit_bool b -> if b then "true" else "false"
  | Parsed_ast.Lit_int i -> string_of_int i
  | Parsed_ast.Lit_string s -> Printf.sprintf "\"%s\"" s

let rec emit_expr ffi_map expr =
  match expr.texpr_desc with
  | TExpr_literal lit -> emit_literal lit
  | TExpr_variable ident -> (
      match StringMap.find_opt ident ffi_map with
      | Some foreign_name -> foreign_name
      | None -> ident)
  | TExpr_call call -> emit_call_expr ffi_map call
  | TExpr_rec { rec_fields; _ } ->
      let field_strs =
        List.map
          (fun (field_name, field_value) ->
            let value_str = emit_expr ffi_map field_value in
            Printf.sprintf "%s: %s" field_name value_str)
          rec_fields
      in
      let fields_str = String.concat ", " field_strs in
      Printf.sprintf "{ %s }" fields_str
  | TExpr_binary_op { binop_left; binop_operator; binop_right } -> (
      let left_str = emit_expr ffi_map binop_left in
      let right_str = emit_expr ffi_map binop_right in
      let operator_str =
        match binop_operator with
        | Binop_add -> "+"
        | Binop_sub -> "-"
        | Binop_mul -> "*"
        | Binop_div -> "/"
        | Binop_eq -> "==="
      in
      match binop_operator with
      | Binop_div ->
          Printf.sprintf "Math.floor(%s %s %s)" left_str operator_str right_str
      | Binop_eq ->
          Printf.sprintf "globalThis.runtime.deepEqual(%s, %s)" left_str
            right_str
      | _ -> Printf.sprintf "(%s %s %s)" left_str operator_str right_str)
  | TExpr_member_access { member_object; member_name; _ } ->
      let object_str = emit_expr ffi_map member_object in
      Printf.sprintf "%s.%s" object_str member_name

and emit_call_expr ffi_map { call_def; call_args } =
  match call_def.texpr_desc with
  | TExpr_member_access { member_object; member_name; is_method = true } ->
      let self_str = emit_expr ffi_map member_object in
      let rec_name =
        match member_object.texpr_type with
        | Types.TRec (name, _) -> name
        | _ -> failwith "Expected member object to be of record type"
      in
      let method_func = Printf.sprintf "rgel_%s_%s" rec_name member_name in
      let arg_strs = List.map (emit_expr ffi_map) call_args in
      let all_args = self_str :: arg_strs in
      Printf.sprintf "%s(%s)" method_func (String.concat ", " all_args)
  | _ ->
      let def_str = emit_expr ffi_map call_def in
      let args_str =
        List.map (emit_expr ffi_map) call_args |> String.concat ", "
      in
      Printf.sprintf "%s(%s)" def_str args_str

and emit_stmt ffi_map stmt =
  match stmt.tstmt_desc with
  | TStmt_expr expr ->
      let expr_str = emit_expr ffi_map expr in
      Printf.sprintf "%s;" expr_str
  | TStmt_var { var_name; var_value; _ } ->
      let value_str = emit_expr ffi_map var_value in
      Printf.sprintf "let %s = %s;" var_name value_str
  | TStmt_return expr ->
      let expr_str = emit_expr ffi_map expr in
      Printf.sprintf "return %s;" expr_str

let emit_def ffi_map def =
  let params_str = List.map fst def.tdef_params |> String.concat ", " in
  let body_stmts =
    List.map (fun stmt -> emit_stmt ffi_map stmt) def.tdef_body
  in
  let body_str = String.concat "\n  " body_stmts in
  Printf.sprintf "function %s(%s) {\n  %s\n}" def.tdef_name params_str body_str

let emit_record_methods ffi_map record =
  let emit_method method_def =
    let params = List.map fst method_def.tdef_params in
    let params_str = "self" :: params |> String.concat ", " in
    let body_stmts =
      List.map (fun stmt -> emit_stmt ffi_map stmt) method_def.tdef_body
    in
    let body_str = String.concat "\n  " body_stmts in
    Printf.sprintf "function rgel_%s_%s(%s) {\n  %s\n}" record.trec_name
      method_def.tdef_name params_str body_str
  in
  List.map emit_method record.trec_methods

let emit_toplevel ffi_map = function
  | ToplevelT_def def -> emit_def ffi_map def
  | ToplevelT_extern _extern -> ""
  | ToplevelT_rec record ->
      emit_record_methods ffi_map record |> String.concat "\n"

let emit_module ffi_map typed_module =
  let toplevels =
    List.map (emit_toplevel ffi_map) typed_module.tmodule_toplevels
    |> List.filter (fun s -> s <> "")
  in
  String.concat "\n" toplevels

let load_extern_names ffi_map toplevel =
  match toplevel with
  | ToplevelT_extern extern ->
      StringMap.add extern.extern_name extern.extern_foreign_name ffi_map
  | _ -> ffi_map

let emit entry typed_module =
  let ffi_map =
    List.fold_left load_extern_names StringMap.empty
      typed_module.tmodule_toplevels
  in
  let bundle_js =
    try
      let ic = open_in "support/dist/bundle.js" in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;
      content ^ "\n"
    with Sys_error _ -> ""
  in
  let module_codegen = emit_module ffi_map typed_module in
  let entry_code = Printf.sprintf "\n%s();" entry in
  bundle_js ^ module_codegen ^ entry_code
