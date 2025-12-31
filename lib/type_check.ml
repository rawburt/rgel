open Parsed_ast
open Typed_ast
module StringMap = Map.Make (String)

let errors = ref []

type context = {
  types : Types.t StringMap.t;
  identifiers : Types.t StringMap.t;
  methods : Types.t StringMap.t StringMap.t;
  return_type : Types.t option;
}

let error error loc = errors := Errors.Type_error (error, loc) :: !errors

let unification loc t1 t2 =
  Debug.trace_log "%s: unification: (%s) and (%s)\n" (Location.show loc)
    (Types.show t1) (Types.show t2);
  if Types.unify t1 t2 then () else error (Errors.Type_mismatch (t1, t2)) loc

let translate_type ctx parsed_type =
  match parsed_type.type_desc with
  | Type_name name -> (
      match StringMap.find_opt name ctx.types with
      | Some t -> t
      | None ->
          error (Errors.Type_not_found name) parsed_type.type_loc;
          Types.fresh_var ())

let check_literal = function
  | Lit_bool _ -> Types.TBool
  | Lit_int _ -> Types.TInt
  | Lit_string _ -> Types.TStr

let rec check_expr ctx (expr : parsed_expr) : typed_expr =
  let check_call_expr (call_expr : Parsed_ast.call_expr) =
    let typed_args = List.map (check_expr ctx) call_expr.call_args in
    let arg_types = List.map (fun arg -> arg.texpr_type) typed_args in
    let typed_def = check_expr ctx call_expr.call_def in
    match typed_def.texpr_type with
    | Types.TDef (_, return_type) ->
        let tvar = Types.fresh_var () in
        unification expr.expr_loc typed_def.texpr_type
          (Types.TDef (arg_types, tvar));
        ( TExpr_call { call_def = typed_def; call_args = typed_args },
          return_type )
    | _ ->
        error (Errors.Not_a_function typed_def.texpr_type) expr.expr_loc;
        ( TExpr_call { call_def = typed_def; call_args = typed_args },
          Types.fresh_var () )
  in

  let check_rec_expr rec_type_name rec_fields =
    let typed_rec_fields =
      List.map (fun (fname, fexpr) -> (fname, check_expr ctx fexpr)) rec_fields
    in
    let rec_field_types =
      List.map
        (fun (fname, fexpr) -> (fname, fexpr.texpr_type))
        typed_rec_fields
    in
    let ty =
      match StringMap.find_opt rec_type_name ctx.types with
      | Some (Types.TRec _ as rec_type) ->
          unification expr.expr_loc rec_type
            (Types.TRec (rec_type_name, ref rec_field_types));
          rec_type
      | Some other_type ->
          error (Errors.Not_a_record other_type) expr.expr_loc;
          Types.fresh_var ()
      | None ->
          error (Errors.Type_not_found rec_type_name) expr.expr_loc;
          Types.fresh_var ()
    in
    (TExpr_rec { rec_fields = typed_rec_fields }, ty)
  in

  let check_member_access member_object member_name =
    let typed_object = check_expr ctx member_object in
    let is_method, field_type =
      match typed_object.texpr_type with
      | Types.TRec (rec_name, fields_ref) -> (
          (* check if it is a record field *)
          match List.assoc_opt member_name !fields_ref with
          | Some field_type -> (false, field_type)
          | None -> (
              (* check if it is a record method *)
              match StringMap.find_opt rec_name ctx.methods with
              | Some method_map -> (
                  match StringMap.find_opt member_name method_map with
                  | Some method_type -> (true, method_type)
                  | None ->
                      error
                        (Errors.Record_field_not_found
                           (member_name, typed_object.texpr_type))
                        expr.expr_loc;
                      (false, Types.fresh_var ()))
              | None ->
                  error
                    (Errors.Record_field_not_found
                       (member_name, typed_object.texpr_type))
                    expr.expr_loc;
                  (false, Types.fresh_var ())))
      | _ ->
          error (Errors.Not_a_record typed_object.texpr_type) expr.expr_loc;
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
          match StringMap.find_opt ident ctx.identifiers with
          | Some t -> Types.instantiate t
          | None ->
              error (Errors.Identifier_not_found ident) expr.expr_loc;
              Types.fresh_var ()
        in
        (TExpr_variable ident, ty)
    | Expr_call call_expr -> check_call_expr call_expr
    | Expr_rec { rec_type_name; rec_fields } ->
        check_rec_expr rec_type_name rec_fields
    | Expr_binary_op { binop_left; binop_operator; binop_right } ->
        let typed_binop_left = check_expr ctx binop_left in
        let typed_binop_right = check_expr ctx binop_right in
        let ty =
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
          ty )
    | Expr_member_access { member_object; member_name } ->
        check_member_access member_object member_name
  in
  { texpr_desc; texpr_type; texpr_loc = expr.expr_loc }

and check_stmt ctx stmt : context * typed_stmt =
  let new_ctx, typed_stmt =
    match stmt.stmt_desc with
    | Stmt_expr expr ->
        let typed_expr = check_expr ctx expr in
        (ctx, TStmt_expr typed_expr)
    | Stmt_var { var_name; var_type; var_value } ->
        if StringMap.mem var_name ctx.identifiers then
          error (Errors.Redeclared_identifier var_name) stmt.stmt_loc;
        let translated_type = translate_type ctx var_type in
        let typed_var_value = check_expr ctx var_value in
        unification stmt.stmt_loc translated_type typed_var_value.texpr_type;
        let identifiers =
          StringMap.add var_name translated_type ctx.identifiers
        in
        ( { ctx with identifiers },
          TStmt_var
            {
              var_name;
              var_type = translated_type;
              var_value = typed_var_value;
            } )
    | Stmt_return expr ->
        let typed_expr = check_expr ctx expr in
        (match ctx.return_type with
        | Some expected_type ->
            unification stmt.stmt_loc expected_type typed_expr.texpr_type
        | None -> error Errors.Return_outside_function stmt.stmt_loc);
        (ctx, TStmt_return typed_expr)
  in
  (new_ctx, { tstmt_desc = typed_stmt; tstmt_loc = stmt.stmt_loc })

and check_block ctx block =
  let _new_ctx, typed_stmts =
    List.fold_left_map check_stmt ctx block.block_stmts
  in
  typed_stmts

and translate_param ctx param =
  let param_type = translate_type ctx param.param_type in
  (param.param_name, param_type)

and check_def ctx def : typed_def =
  Debug.trace_log "%s: checking: def %s\n"
    (Location.show def.def_loc)
    def.def_name;
  let translated_params = List.map (translate_param ctx) def.def_params in
  let identifiers_with_params =
    List.fold_left
      (fun id_map (name, ty) -> StringMap.add name ty id_map)
      ctx.identifiers translated_params
  in
  let return_type = translate_type ctx def.def_return_type in
  let ctx_with_params_and_return_type =
    {
      ctx with
      identifiers = identifiers_with_params;
      return_type = Some return_type;
    }
  in
  let typed_stmts = check_block ctx_with_params_and_return_type def.def_body in
  {
    tdef_name = def.def_name;
    tdef_params = translated_params;
    tdef_return_type = return_type;
    tdef_body = typed_stmts;
    tdef_loc = def.def_loc;
  }

and check_record ctx record : typed_rec =
  Debug.trace_log "%s: checking: rec %s\n"
    (Location.show record.rec_loc)
    record.rec_name;
  match StringMap.find_opt record.rec_name ctx.types with
  | Some (Types.TRec (rec_name, fields_ref)) ->
      (* check fields *)
      let check_field field =
        let field_type = translate_type ctx field.field_type in
        (field.field_name, field_type)
      in
      fields_ref := List.map check_field record.rec_fields;
      (* check methods  *)
      let record_type = Types.TRec (rec_name, fields_ref) in
      let ctx_with_self =
        {
          ctx with
          identifiers = StringMap.add "self" record_type ctx.identifiers;
        }
      in
      let check_method method_def = check_def ctx_with_self method_def in
      let typed_methods = List.map check_method record.rec_methods in
      {
        trec_name = record.rec_name;
        trec_fields = !fields_ref;
        trec_methods = typed_methods;
        trec_loc = record.rec_loc;
      }
  | _ -> failwith "Unreachable: record type not found in context"

and check_toplevel ctx toplevel : context * typed_toplevel =
  match toplevel with
  | Toplevel_def def ->
      let typed_def = check_def ctx def in
      (ctx, ToplevelT_def typed_def)
  | Toplevel_extern extern -> (ctx, ToplevelT_extern extern)
  | Toplevel_rec record ->
      let typed_record = check_record ctx record in
      (ctx, ToplevelT_rec typed_record)

and load_def ctx def =
  Debug.trace_log "%s: loading: def %s\n"
    (Location.show def.def_loc)
    def.def_name;
  if StringMap.mem def.def_name ctx.identifiers then
    error (Errors.Redeclared_identifier def.def_name) def.def_loc;
  let translated_params = List.map (translate_param ctx) def.def_params in
  let param_types = List.map snd translated_params in
  let return_type = Types.TUnit in
  let def_type = Types.TDef (param_types, return_type) in
  let identifiers = StringMap.add def.def_name def_type ctx.identifiers in
  { ctx with identifiers }

and load_extern ctx extern =
  Debug.trace_log "%s: loading: extern %s\n"
    (Location.show extern.extern_loc)
    extern.extern_name;
  if StringMap.mem extern.extern_name ctx.identifiers then
    error (Errors.Redeclared_identifier extern.extern_name) extern.extern_loc;
  let types_with_params =
    List.fold_left
      (fun acc name -> StringMap.add name (Types.TParam name) acc)
      ctx.types extern.extern_type_params
  in
  let ctx_with_type_params = { ctx with types = types_with_params } in
  let param_types =
    List.map (translate_type ctx_with_type_params) extern.extern_params
  in
  let return_type =
    translate_type ctx_with_type_params extern.extern_return_type
  in
  let extern_type = Types.TDef (param_types, return_type) in
  let identifiers =
    StringMap.add extern.extern_name extern_type ctx.identifiers
  in
  { ctx with identifiers }

and load_record ctx record =
  Debug.trace_log "%s: loading: rec %s\n"
    (Location.show record.rec_loc)
    record.rec_name;
  if StringMap.mem record.rec_name ctx.types then
    error (Errors.Redeclared_identifier record.rec_name) record.rec_loc;
  let record_type = Types.TRec (record.rec_name, ref []) in
  let types = StringMap.add record.rec_name record_type ctx.types in
  let ctx_with_type = { ctx with types } in
  (* load methods *)
  let load_method method_def =
    let translated_params =
      List.map (translate_param ctx_with_type) method_def.def_params
    in
    let param_types = List.map snd translated_params in
    let return_type = translate_type ctx_with_type method_def.def_return_type in
    (* self is first param when checking locally but not to the external world so its left out here *)
    let method_type = Types.TDef (param_types, return_type) in
    (method_def.def_name, method_type)
  in
  let method_map =
    List.fold_left
      (fun map (name, ty) -> StringMap.add name ty map)
      StringMap.empty
      (List.map load_method record.rec_methods)
  in
  let methods = StringMap.add record.rec_name method_map ctx.methods in
  { ctx_with_type with methods }

and load_toplevel ctx = function
  | Toplevel_def def -> load_def ctx def
  | Toplevel_extern extern -> load_extern ctx extern
  | Toplevel_rec record -> load_record ctx record

let check entry parsed_module =
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
      methods = StringMap.empty;
      return_type = None;
    }
  in
  let type_decls, defs =
    List.partition
      (function
        | Toplevel_extern _ | Toplevel_rec _ -> true | Toplevel_def _ -> false)
      parsed_module.module_toplevels
  in
  (* process in order: type declarations first, then definitions *)
  let ordered_toplevels = type_decls @ defs in
  let ctx_with_types =
    List.fold_left load_toplevel initial_ctx ordered_toplevels
  in
  if not (StringMap.mem entry ctx_with_types.identifiers) then
    error (Errors.Entry_not_found entry) Location.none;
  let _ctx, typed_toplevels =
    List.fold_left_map check_toplevel ctx_with_types ordered_toplevels
  in
  let typed_module =
    {
      tmodule_name = parsed_module.module_name;
      tmodule_toplevels = typed_toplevels;
    }
  in
  if !errors = [] then Ok typed_module else Error (List.rev !errors)
