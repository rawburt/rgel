open Parsed_ast
module StringMap = Map.Make (String)

let emit_literal = function
  | Lit_bool b -> if b then "true" else "false"
  | Lit_int i -> string_of_int i
  | Lit_string s -> Printf.sprintf "\"%s\"" s

let rec emit_expr ffi_map expr =
  match expr.expr_desc with
  | Expr_literal lit -> emit_literal lit
  | Expr_ident ident -> (
      match StringMap.find_opt ident ffi_map with
      | Some foreign_name -> foreign_name
      | None -> ident)
  | Expr_call call -> emit_call_expr ffi_map call
  | Expr_rec { rec_fields; _ } ->
      let field_strs =
        List.map
          (fun (field_name, field_value) ->
            let value_str = emit_expr ffi_map field_value in
            Printf.sprintf "%s: %s" field_name value_str)
          rec_fields
      in
      let fields_str = String.concat ", " field_strs in
      Printf.sprintf "{ %s }" fields_str
  | Expr_binary_op { binop_left; binop_operator; binop_right } -> (
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

and emit_call_expr ffi_map { call_def; call_args } =
  let def_str = emit_expr ffi_map call_def in
  let args_str = List.map (emit_expr ffi_map) call_args |> String.concat ", " in
  Printf.sprintf "%s(%s)" def_str args_str

let emit_def ffi_map def =
  let params_str =
    List.map (fun param -> param.param_name) def.def_params
    |> String.concat ", "
  in
  let body_stmts =
    List.map
      (fun stmt ->
        match stmt.stmt_desc with
        | Stmt_expr expr -> emit_expr ffi_map expr ^ ";")
      def.def_body.block_stmts
  in
  let body_str = String.concat "\n  " body_stmts in
  Printf.sprintf "function %s(%s) {\n  %s\n}" def.def_name params_str body_str

let emit_toplevel ffi_map = function
  | Toplevel_def def -> emit_def ffi_map def
  | Toplevel_extern _extern -> ""
  | Toplevel_rec _record -> ""

let emit_module ffi_map parsed_module =
  let toplevels =
    List.map (emit_toplevel ffi_map) parsed_module.module_toplevels
    |> List.filter (fun s -> s <> "")
  in
  String.concat "\n" toplevels

let load_extern_names ffi_map toplevel =
  match toplevel with
  | Parsed_ast.Toplevel_extern extern ->
      StringMap.add extern.extern_name extern.extern_foreign_name ffi_map
  | _ -> ffi_map

let emit entry parsed_module =
  let ffi_map =
    List.fold_left load_extern_names StringMap.empty
      parsed_module.module_toplevels
  in
  let bundle_js =
    try
      let ic = open_in "support/dist/bundle.js" in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;
      content ^ "\n"
    with Sys_error _ -> ""
  in
  let module_codegen = emit_module ffi_map parsed_module in
  let entry_code = Printf.sprintf "\n%s();" entry in
  bundle_js ^ module_codegen ^ entry_code
