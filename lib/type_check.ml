open Parsed_ast
module StringMap = Map.Make (String)

let errors = ref []

type context = {
  types : Types.t StringMap.t;
  identifiers : Types.t StringMap.t;
}

let error error loc = errors := Errors.Type_error (error, loc) :: !errors

let unification loc t1 t2 =
  if Types.unify t1 t2 then () else error (Errors.Type_mismatch (t1, t2)) loc

let translate_type ctx parsed_type =
  match parsed_type.type_desc with
  | Type_name name -> (
      match StringMap.find_opt name ctx.types with
      | Some t -> t
      | None ->
          error (Errors.Type_not_found name) parsed_type.type_loc;
          Types.TVar (ref None))

let check_literal = function
  | Lit_bool _ -> Types.TBool
  | Lit_int _ -> Types.TInt
  | Lit_string _ -> Types.TStr

let rec check_expr ctx expr =
  let check_call_expr call_expr =
    let arg_types =
      List.map (fun arg -> check_expr ctx arg) call_expr.call_args
    in
    let def_type = check_expr ctx call_expr.call_def in
    match def_type with
    | Types.TDef (_, return_type) ->
        unification expr.expr_loc def_type
          (Types.TDef (arg_types, Types.TVar (ref None)));
        return_type
    | _ ->
        error (Errors.Not_a_function def_type) expr.expr_loc;
        Types.TVar (ref None)
  in
  match expr.expr_desc with
  | Expr_literal lit -> check_literal lit
  | Expr_ident ident -> (
      match StringMap.find_opt ident ctx.identifiers with
      | Some t -> t
      | None ->
          error (Errors.Identifier_not_found ident) expr.expr_loc;
          Types.TVar (ref None))
  | Expr_call call_expr -> check_call_expr call_expr

and check_stmt ctx stmt =
  match stmt.stmt_desc with
  | Stmt_expr expr ->
      let _ = check_expr ctx expr in
      ctx

and check_block ctx block = List.fold_left check_stmt ctx block.block_stmts

and translate_param ctx param =
  let param_type = translate_type ctx param.param_type in
  (param.param_name, param_type)

and check_def ctx def =
  let translated_params = List.map (translate_param ctx) def.def_params in
  let identifiers_with_params =
    List.fold_left
      (fun id_map (name, ty) -> StringMap.add name ty id_map)
      ctx.identifiers translated_params
  in
  let ctx_with_params = { ctx with identifiers = identifiers_with_params } in
  ignore (check_block ctx_with_params def.def_body)

and check_toplevel ctx = function
  | Toplevel_def def -> check_def ctx def
  | Toplevel_extern _extern -> ()

and load_def ctx def =
  let translated_params = List.map (translate_param ctx) def.def_params in
  let param_types = List.map snd translated_params in
  let return_type = Types.TUnit in
  let def_type = Types.TDef (param_types, return_type) in
  { ctx with types = StringMap.add def.def_name def_type ctx.types }

and load_extern ctx extern =
  let param_types = List.map (translate_type ctx) extern.extern_params in
  let return_type = translate_type ctx extern.extern_return_type in
  let extern_type = Types.TDef (param_types, return_type) in
  let identifiers =
    StringMap.add extern.extern_name extern_type ctx.identifiers
  in
  { ctx with identifiers }

and load_toplevel ctx = function
  | Toplevel_def def -> load_def ctx def
  | Toplevel_extern extern -> load_extern ctx extern

let check parsed_module =
  let initial_ctx =
    {
      types =
        [
          ("unit", Types.TUnit);
          ("bool", Types.TBool);
          ("int", Types.TInt);
          ("str", Types.TStr);
        ]
        |> StringMap.of_list;
      identifiers = StringMap.empty;
    }
  in
  let ctx_with_types =
    List.fold_left load_toplevel initial_ctx parsed_module.module_toplevels
  in
  let _ =
    List.iter (check_toplevel ctx_with_types) parsed_module.module_toplevels
  in
  if !errors = [] then Ok () else Error (List.rev !errors)
