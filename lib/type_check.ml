open Parsed_ast
module StringMap = Map.Make (String)

let errors = ref []

type context = {
  types : Types.t StringMap.t;
  identifiers : Types.t StringMap.t;
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

let rec check_expr ctx expr =
  let check_call_expr call_expr =
    let arg_types =
      List.map (fun arg -> check_expr ctx arg) call_expr.call_args
    in
    let def_type = check_expr ctx call_expr.call_def in
    match def_type with
    | Types.TDef (_, return_type) ->
        let tvar = Types.fresh_var () in
        unification expr.expr_loc def_type (Types.TDef (arg_types, tvar));
        return_type
    | _ ->
        error (Errors.Not_a_function def_type) expr.expr_loc;
        Types.fresh_var ()
  in
  let check_rec_expr rec_type_name rec_fields =
    match StringMap.find_opt rec_type_name ctx.types with
    | Some (Types.TRec _ as rec_type) ->
        let rec_fields_types =
          List.map
            (fun (fname, fexpr) -> (fname, check_expr ctx fexpr))
            rec_fields
        in
        unification expr.expr_loc rec_type
          (Types.TRec (rec_type_name, ref rec_fields_types));
        rec_type
    | Some other_type ->
        error (Errors.Not_a_record other_type) expr.expr_loc;
        Types.fresh_var ()
    | None ->
        error (Errors.Type_not_found rec_type_name) expr.expr_loc;
        Types.fresh_var ()
  in
  match expr.expr_desc with
  | Expr_literal lit -> check_literal lit
  | Expr_ident ident -> (
      match StringMap.find_opt ident ctx.identifiers with
      | Some t -> Types.instantiate t
      | None ->
          error (Errors.Identifier_not_found ident) expr.expr_loc;
          Types.fresh_var ())
  | Expr_call call_expr -> check_call_expr call_expr
  | Expr_rec { rec_type_name; rec_fields } ->
      check_rec_expr rec_type_name rec_fields
  | Expr_binary_op { binop_left; binop_operator; binop_right } -> (
      let left_type = check_expr ctx binop_left in
      let right_type = check_expr ctx binop_right in
      match binop_operator with
      | Binop_add ->
          let expected_type =
            match left_type with Types.TStr -> Types.TStr | _ -> Types.TInt
          in
          unification expr.expr_loc left_type expected_type;
          unification expr.expr_loc right_type expected_type;
          expected_type
      | Binop_sub | Binop_mul | Binop_div ->
          unification expr.expr_loc left_type Types.TInt;
          unification expr.expr_loc right_type Types.TInt;
          Types.TInt
      | Binop_eq ->
          unification expr.expr_loc left_type right_type;
          Types.TBool)

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
  Debug.trace_log "%s: checking: def %s\n"
    (Location.show def.def_loc)
    def.def_name;
  let translated_params = List.map (translate_param ctx) def.def_params in
  let identifiers_with_params =
    List.fold_left
      (fun id_map (name, ty) -> StringMap.add name ty id_map)
      ctx.identifiers translated_params
  in
  let ctx_with_params = { ctx with identifiers = identifiers_with_params } in
  ignore (check_block ctx_with_params def.def_body)

and check_record ctx record =
  Debug.trace_log "%s: checking: rec %s\n"
    (Location.show record.rec_loc)
    record.rec_name;
  match StringMap.find_opt record.rec_name ctx.types with
  | Some (Types.TRec (_, fields_ref)) ->
      let check_field field =
        let field_type = translate_type ctx field.field_type in
        (field.field_name, field_type)
      in
      fields_ref := List.map check_field record.rec_fields;
      ctx
  | _ -> failwith "Unreachable: record type not found in context"

and check_toplevel ctx = function
  | Toplevel_def def ->
      check_def ctx def;
      ctx
  | Toplevel_extern _extern -> ctx
  | Toplevel_rec record -> check_record ctx record

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
  { ctx with types }

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
  let _ = List.fold_left check_toplevel ctx_with_types ordered_toplevels in
  if !errors = [] then Ok () else Error (List.rev !errors)
