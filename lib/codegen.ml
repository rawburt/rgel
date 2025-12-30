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
  | Toplevel_extern _ -> ""

let emit_module ffi_map parsed_module =
  let toplevels =
    List.map (emit_toplevel ffi_map) parsed_module.module_toplevels
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
  let module_codegen = emit_module ffi_map parsed_module in
  let entry_code = Printf.sprintf "\n%s();" entry in
  module_codegen ^ entry_code
