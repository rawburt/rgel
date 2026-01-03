open Parsed_ast
open Typed_ast
open Type_common

let unification loc t1 t2 =
  Debug.trace_log "%s: unification: (%s) and (%s)\n" (Location.show loc)
    (Types.show t1) (Types.show t2);
  if Types.unify t1 t2 then ()
  else Env.error (Errors.Type_mismatch (t1, t2)) loc

let check_literal = function
  | Lit_bool _ -> Types.TBool
  | Lit_int _ -> Types.TInt
  | Lit_string _ -> Types.TStr

let rec check_call_expr env (call_expr : Parsed_ast.call_expr) loc =
  let typed_args = List.map (check_expr env) call_expr.call_args in
  let arg_types = List.map (fun arg -> arg.texpr_type) typed_args in
  let typed_def = check_expr env call_expr.call_def in
  match typed_def.texpr_type with
  | Types.TDef (_, return_type) ->
      let tvar = Types.fresh_var () in
      unification loc typed_def.texpr_type (Types.TDef (arg_types, tvar));
      (TExpr_call { call_def = typed_def; call_args = typed_args }, return_type)
  | _ ->
      Env.error (Errors.Not_a_function typed_def.texpr_type) loc;
      ( TExpr_call { call_def = typed_def; call_args = typed_args },
        Types.fresh_var () )

and check_rec_expr env rec_type_name rec_fields loc =
  let typed_rec_fields =
    List.map (fun (fname, fexpr) -> (fname, check_expr env fexpr)) rec_fields
  in
  let result_ty =
    match Env.find_record env rec_type_name with
    | Some expected_fields ->
        let check_field (fname, fexpr) =
          match List.assoc_opt fname expected_fields with
          | Some expected_type ->
              unification loc fexpr.texpr_type expected_type;
              true
          | None -> false
        in
        if
          (not (List.for_all check_field typed_rec_fields))
          || List.length expected_fields <> List.length typed_rec_fields
        then
          Env.error
            (Errors.Record_field_mismatch
               {
                 record_name = rec_type_name;
                 expected_fields;
                 actual_fields =
                   typed_rec_fields
                   |> List.map (fun (n, e) -> (n, e.texpr_type));
               })
            loc;
        Types.TRec rec_type_name
    | None ->
        Env.error (Errors.Type_not_found rec_type_name) loc;
        Types.fresh_var ()
  in
  (TExpr_rec { rec_fields = typed_rec_fields }, result_ty)

and check_member_access env member_object member_name loc =
  let typed_object = check_expr env member_object in
  let is_method, field_type =
    match typed_object.texpr_type with
    | Types.TRec rec_name -> (
        (* check if it is a record field *)
        let fields =
          match Env.find_record env rec_name with
          | Some flds -> flds
          | None -> []
        in
        match List.assoc_opt member_name fields with
        | Some field_type -> (false, field_type)
        | None -> (
            match Env.find_method env rec_name member_name with
            | Some method_type -> (true, method_type)
            | None ->
                Env.error
                  (Errors.Record_field_not_found
                     (member_name, typed_object.texpr_type))
                  loc;
                (false, Types.fresh_var ())))
    | _ ->
        Env.error (Errors.Not_a_record typed_object.texpr_type) loc;
        (false, Types.fresh_var ())
  in
  ( TExpr_member_access { member_object = typed_object; member_name; is_method },
    field_type )

and check_expr env (expr : parsed_expr) : typed_expr =
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
    | Expr_call call_expr -> check_call_expr env call_expr expr.expr_loc
    | Expr_rec { rec_type_name; rec_fields } ->
        check_rec_expr env rec_type_name rec_fields expr.expr_loc
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
        check_member_access env member_object member_name expr.expr_loc
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

and check_record env record : Env.t * typed_rec =
  Debug.trace_log "%s: checking: rec %s\n"
    (Location.show record.rec_loc)
    record.rec_name;
  (* check fields *)
  let check_field field =
    let field_type = translate_type env field.field_type in
    (field.field_name, field_type)
  in
  let fields = List.map check_field record.rec_fields in
  let env_with_record = Env.add_record env record.rec_name fields in
  (* check methods  *)
  let record_type = Types.TRec record.rec_name in
  let env_with_self = Env.add_local env_with_record "self" record_type in
  let check_method method_def = check_def env_with_self method_def in
  let typed_methods = List.map check_method record.rec_methods in
  ( env_with_record,
    {
      trec_name = record.rec_name;
      trec_fields = fields;
      trec_methods = typed_methods;
      trec_loc = record.rec_loc;
    } )

and check_toplevel env toplevel : Env.t * typed_toplevel =
  match toplevel with
  | Toplevel_def def ->
      let typed_def = check_def env def in
      (env, ToplevelT_def typed_def)
  | Toplevel_extern extern -> (env, ToplevelT_extern extern)
  | Toplevel_rec record ->
      let env', typed_record = check_record env record in
      (env', ToplevelT_rec typed_record)

let check env parsed_module =
  let top = ordered_toplevels parsed_module.module_toplevels in
  let _final_env, typed_toplevels = List.fold_left_map check_toplevel env top in
  let typed_module =
    {
      tmodule_name = parsed_module.module_name;
      tmodule_toplevels = typed_toplevels;
    }
  in
  let errors = Env.get_errors () in
  if errors = [] then Ok typed_module else Error (List.rev errors)
