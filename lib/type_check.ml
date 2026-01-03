open Parsed_ast
open Typed_ast

let unification loc t1 t2 =
  Debug.trace_log "%s: unification: (%s) and (%s)\n" (Location.show loc)
    (Types.show t1) (Types.show t2);
  if Types.unify t1 t2 then ()
  else Env.error (Errors.Type_mismatch (t1, t2)) loc

let translate_type env parsed_type =
  match parsed_type.type_desc with
  | Type_name name -> (
      match Env.find_type env name with
      | Some t -> t
      | None ->
          Env.error (Errors.Type_not_found name) parsed_type.type_loc;
          Types.fresh_var ())

let check_literal = function
  | Lit_bool _ -> Types.TBool
  | Lit_int _ -> Types.TInt
  | Lit_string _ -> Types.TStr

let rec check_expr env (expr : parsed_expr) : typed_expr =
  let check_call_expr (call_expr : Parsed_ast.call_expr) =
    let typed_args = List.map (check_expr env) call_expr.call_args in
    let arg_types = List.map (fun arg -> arg.texpr_type) typed_args in
    let typed_def = check_expr env call_expr.call_def in
    match typed_def.texpr_type with
    | Types.TDef (_, return_type) ->
        let tvar = Types.fresh_var () in
        unification expr.expr_loc typed_def.texpr_type
          (Types.TDef (arg_types, tvar));
        ( TExpr_call { call_def = typed_def; call_args = typed_args },
          return_type )
    | _ ->
        Env.error (Errors.Not_a_function typed_def.texpr_type) expr.expr_loc;
        ( TExpr_call { call_def = typed_def; call_args = typed_args },
          Types.fresh_var () )
  in

  let check_rec_expr rec_type_name rec_fields =
    let typed_rec_fields =
      List.map (fun (fname, fexpr) -> (fname, check_expr env fexpr)) rec_fields
    in
    let rec_field_types =
      List.map
        (fun (fname, fexpr) -> (fname, fexpr.texpr_type))
        typed_rec_fields
    in
    let result_ty =
      match Env.find_type env rec_type_name with
      | Some (Types.TRec _ as rec_type) ->
          unification expr.expr_loc rec_type
            (Types.TRec (rec_type_name, ref rec_field_types));
          rec_type
      | Some other_type ->
          Env.error (Errors.Not_a_record other_type) expr.expr_loc;
          Types.fresh_var ()
      | None ->
          Env.error (Errors.Type_not_found rec_type_name) expr.expr_loc;
          Types.fresh_var ()
    in
    (TExpr_rec { rec_fields = typed_rec_fields }, result_ty)
  in

  let check_member_access member_object member_name =
    let typed_object = check_expr env member_object in
    let is_method, field_type =
      match typed_object.texpr_type with
      | Types.TRec (rec_name, fields_ref) -> (
          (* check if it is a record field *)
          match List.assoc_opt member_name !fields_ref with
          | Some field_type -> (false, field_type)
          | None -> (
              match Env.find_method env rec_name member_name with
              | Some method_type -> (true, method_type)
              | None ->
                  Env.error
                    (Errors.Record_field_not_found
                       (member_name, typed_object.texpr_type))
                    expr.expr_loc;
                  (false, Types.fresh_var ())))
      | _ ->
          Env.error (Errors.Not_a_record typed_object.texpr_type) expr.expr_loc;
          (false, Types.fresh_var ())
    in
    ( TExpr_member_access
        { member_object = typed_object; member_name; is_method },
      field_type )
  in

  let texpr_desc, texpr_type =
    match expr.expr_desc with
    | Expr_literal lit -> (TExpr_literal lit, check_literal lit)
    | Expr_variable ident ->
        let ty =
          match Env.find_local env ident with
          | Some t -> Types.instantiate t
          | None ->
              Env.error (Errors.Identifier_not_found ident) expr.expr_loc;
              Types.fresh_var ()
        in
        (TExpr_variable ident, ty)
    | Expr_call call_expr -> check_call_expr call_expr
    | Expr_rec { rec_type_name; rec_fields } ->
        check_rec_expr rec_type_name rec_fields
    | Expr_binary_op { binop_left; binop_operator; binop_right } ->
        let typed_binop_left = check_expr env binop_left in
        let typed_binop_right = check_expr env binop_right in
        let result_ty =
          match binop_operator with
          | Binop_add ->
              let expected_type =
                match typed_binop_left.texpr_type with
                | Types.TStr -> Types.TStr
                | _ -> Types.TInt
              in
              unification expr.expr_loc typed_binop_left.texpr_type
                expected_type;
              unification expr.expr_loc typed_binop_right.texpr_type
                expected_type;
              expected_type
          | Binop_sub | Binop_mul | Binop_div ->
              unification expr.expr_loc typed_binop_left.texpr_type Types.TInt;
              unification expr.expr_loc typed_binop_right.texpr_type Types.TInt;
              Types.TInt
          | Binop_eq ->
              unification expr.expr_loc typed_binop_left.texpr_type
                typed_binop_right.texpr_type;
              Types.TBool
        in
        ( TExpr_binary_op
            {
              binop_left = typed_binop_left;
              binop_operator;
              binop_right = typed_binop_right;
            },
          result_ty )
    | Expr_member_access { member_object; member_name } ->
        check_member_access member_object member_name
  in
  { texpr_desc; texpr_type; texpr_loc = expr.expr_loc }

and check_stmt env stmt : Env.t * typed_stmt =
  let new_env, typed_stmt =
    match stmt.stmt_desc with
    | Stmt_expr expr ->
        let typed_expr = check_expr env expr in
        (env, TStmt_expr typed_expr)
    | Stmt_var { var_name; var_type; var_value } ->
        if Env.mem_local env var_name then
          Env.error (Errors.Redeclared_identifier var_name) stmt.stmt_loc;
        let translated_type = translate_type env var_type in
        let typed_var_value = check_expr env var_value in
        unification stmt.stmt_loc translated_type typed_var_value.texpr_type;
        let env' = Env.add_local env var_name translated_type in
        ( env',
          TStmt_var
            {
              var_name;
              var_type = translated_type;
              var_value = typed_var_value;
            } )
    | Stmt_return expr ->
        let typed_expr = check_expr env expr in
        (match Env.get_return_type env with
        | Some expected_type ->
            unification stmt.stmt_loc expected_type typed_expr.texpr_type
        | None -> Env.error Errors.Return_outside_function stmt.stmt_loc);
        (env, TStmt_return typed_expr)
  in
  (new_env, { tstmt_desc = typed_stmt; tstmt_loc = stmt.stmt_loc })

and check_block env block =
  let _new_env, typed_stmts =
    List.fold_left_map check_stmt env block.block_stmts
  in
  typed_stmts

and translate_param env param =
  let param_type = translate_type env param.param_type in
  (param.param_name, param_type)

and check_def env def : typed_def =
  Debug.trace_log "%s: checking: def %s\n"
    (Location.show def.def_loc)
    def.def_name;
  let translated_params = List.map (translate_param env) def.def_params in
  let env_with_params =
    List.fold_left
      (fun env (name, ty) -> Env.add_local env name ty)
      env translated_params
  in
  let return_type = translate_type env def.def_return_type in
  let env_with_params_and_return_type =
    Env.set_return_type env_with_params return_type
  in
  let typed_stmts = check_block env_with_params_and_return_type def.def_body in
  {
    tdef_name = def.def_name;
    tdef_params = translated_params;
    tdef_return_type = return_type;
    tdef_body = typed_stmts;
    tdef_loc = def.def_loc;
  }

and check_record env record : typed_rec =
  Debug.trace_log "%s: checking: rec %s\n"
    (Location.show record.rec_loc)
    record.rec_name;
  match Env.find_type env record.rec_name with
  | Some (Types.TRec (rec_name, fields_ref)) ->
      (* check fields *)
      let check_field field =
        let field_type = translate_type env field.field_type in
        (field.field_name, field_type)
      in
      fields_ref := List.map check_field record.rec_fields;
      (* check methods  *)
      let record_type = Types.TRec (rec_name, fields_ref) in
      let env_with_self = Env.add_local env "self" record_type in
      let check_method method_def = check_def env_with_self method_def in
      let typed_methods = List.map check_method record.rec_methods in
      {
        trec_name = record.rec_name;
        trec_fields = !fields_ref;
        trec_methods = typed_methods;
        trec_loc = record.rec_loc;
      }
  | _ -> failwith "Unreachable: record type not found in env"

and check_toplevel env toplevel : Env.t * typed_toplevel =
  match toplevel with
  | Toplevel_def def ->
      let typed_def = check_def env def in
      (env, ToplevelT_def typed_def)
  | Toplevel_extern extern -> (env, ToplevelT_extern extern)
  | Toplevel_rec record ->
      let typed_record = check_record env record in
      (env, ToplevelT_rec typed_record)

and load_def env def =
  Debug.trace_log "%s: loading: def %s\n"
    (Location.show def.def_loc)
    def.def_name;
  if Env.mem_local env def.def_name then
    Env.error (Errors.Redeclared_identifier def.def_name) def.def_loc;
  let translated_params = List.map (translate_param env) def.def_params in
  let param_types = List.map snd translated_params in
  let return_type = translate_type env def.def_return_type in
  let def_type = Types.TDef (param_types, return_type) in
  Env.add_local env def.def_name def_type

and load_extern env extern =
  Debug.trace_log "%s: loading: extern %s\n"
    (Location.show extern.extern_loc)
    extern.extern_name;
  if Env.mem_local env extern.extern_name then
    Env.error (Errors.Redeclared_identifier extern.extern_name)
      extern.extern_loc;
  let env_with_type_params =
    List.fold_left
      (fun env name -> Env.add_type env name (Types.TParam name))
      env extern.extern_type_params
  in
  let param_types =
    List.map (translate_type env_with_type_params) extern.extern_params
  in
  let return_type =
    translate_type env_with_type_params extern.extern_return_type
  in
  let extern_type = Types.TDef (param_types, return_type) in
  Env.add_local env extern.extern_name extern_type

and load_record env record =
  Debug.trace_log "%s: loading: rec %s\n"
    (Location.show record.rec_loc)
    record.rec_name;
  if Env.mem_type env record.rec_name then
    Env.error (Errors.Redeclared_identifier record.rec_name) record.rec_loc;
  let record_type = Types.TRec (record.rec_name, ref []) in
  let env_with_type = Env.add_type env record.rec_name record_type in
  (* load methods *)
  let load_method method_def =
    let translated_params =
      List.map (translate_param env_with_type) method_def.def_params
    in
    let param_types = List.map snd translated_params in
    let return_type = translate_type env_with_type method_def.def_return_type in
    (* self is first param when checking locally but not to the external world so its left out here *)
    let method_type = Types.TDef (param_types, return_type) in
    (method_def.def_name, method_type)
  in
  let env_with_methods =
    List.fold_left
      (fun env (name, ty) -> Env.add_method env record.rec_name name ty)
      env_with_type
      (List.map load_method record.rec_methods)
  in
  env_with_methods

and load_toplevel env = function
  | Toplevel_def def -> load_def env def
  | Toplevel_extern extern -> load_extern env extern
  | Toplevel_rec record -> load_record env record

let check entry parsed_module =
  let initial_env = Env.create () in
  let type_decls, defs =
    List.partition
      (function
        | Toplevel_extern _ | Toplevel_rec _ -> true | Toplevel_def _ -> false)
      parsed_module.module_toplevels
  in
  (* process in order: type declarations first, then definitions *)
  let ordered_toplevels = type_decls @ defs in
  let env_with_types =
    List.fold_left load_toplevel initial_env ordered_toplevels
  in
  if not (Env.mem_local env_with_types entry) then
    Env.error (Errors.Entry_not_found entry) Location.none;
  let _env, typed_toplevels =
    List.fold_left_map check_toplevel env_with_types ordered_toplevels
  in
  let typed_module =
    {
      tmodule_name = parsed_module.module_name;
      tmodule_toplevels = typed_toplevels;
    }
  in
  let errors = Env.get_errors () in
  if errors = [] then Ok typed_module else Error (List.rev errors)
