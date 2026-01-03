open Parsed_ast
open Typed_ast

(* -------------------------------------------- *)

module Context = struct
  module StringMap = Map.Make (String)

  type t = {
    types : Types.t StringMap.t;
    locals : Types.t StringMap.t;
    methods : Types.t StringMap.t StringMap.t;
    return_type : Types.t option;
  }

  let errors = ref []

  let create () =
    errors := [];
    {
      types = Types.primitives |> StringMap.of_list;
      locals = StringMap.empty;
      methods = StringMap.empty;
      return_type = None;
    }

  let error error loc = errors := Errors.Type_error (error, loc) :: !errors
  let find_type ctx name = StringMap.find_opt name ctx.types
  let find_methods ctx name = StringMap.find_opt name ctx.methods

  let find_method ctx rec_name method_name =
    match find_methods ctx rec_name with
    | Some method_map -> StringMap.find_opt method_name method_map
    | None -> None

  let find_local ctx name = StringMap.find_opt name ctx.locals
  let mem_local ctx name = StringMap.mem name ctx.locals
  let mem_type ctx name = StringMap.mem name ctx.types

  let add_local ctx name ty =
    { ctx with locals = StringMap.add name ty ctx.locals }

  let add_type ctx name ty =
    { ctx with types = StringMap.add name ty ctx.types }

  let add_method ctx rec_name method_name ty =
    let method_map =
      match find_methods ctx rec_name with
      | Some map -> map
      | None -> StringMap.empty
    in
    let updated_method_map = StringMap.add method_name ty method_map in
    let updated_methods =
      StringMap.add rec_name updated_method_map ctx.methods
    in
    { ctx with methods = updated_methods }

  let set_return_type ctx return_type = { ctx with return_type }
end

(* -------------------------------------------- *)

let unification loc t1 t2 =
  Debug.trace_log "%s: unification: (%s) and (%s)\n" (Location.show loc)
    (Types.show t1) (Types.show t2);
  if Types.unify t1 t2 then ()
  else Context.error (Errors.Type_mismatch (t1, t2)) loc

let translate_type ctx parsed_type =
  match parsed_type.type_desc with
  | Type_name name -> (
      match Context.find_type ctx name with
      | Some t -> t
      | None ->
          Context.error (Errors.Type_not_found name) parsed_type.type_loc;
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
        Context.error (Errors.Not_a_function typed_def.texpr_type) expr.expr_loc;
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
    let result_ty =
      match Context.find_type ctx rec_type_name with
      | Some (Types.TRec _ as rec_type) ->
          unification expr.expr_loc rec_type
            (Types.TRec (rec_type_name, ref rec_field_types));
          rec_type
      | Some other_type ->
          Context.error (Errors.Not_a_record other_type) expr.expr_loc;
          Types.fresh_var ()
      | None ->
          Context.error (Errors.Type_not_found rec_type_name) expr.expr_loc;
          Types.fresh_var ()
    in
    (TExpr_rec { rec_fields = typed_rec_fields }, result_ty)
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
              match Context.find_method ctx rec_name member_name with
              | Some method_type -> (true, method_type)
              | None ->
                  Context.error
                    (Errors.Record_field_not_found
                       (member_name, typed_object.texpr_type))
                    expr.expr_loc;
                  (false, Types.fresh_var ())))
      | _ ->
          Context.error (Errors.Not_a_record typed_object.texpr_type)
            expr.expr_loc;
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
          match Context.find_local ctx ident with
          | Some t -> Types.instantiate t
          | None ->
              Context.error (Errors.Identifier_not_found ident) expr.expr_loc;
              Types.fresh_var ()
        in
        (TExpr_variable ident, ty)
    | Expr_call call_expr -> check_call_expr call_expr
    | Expr_rec { rec_type_name; rec_fields } ->
        check_rec_expr rec_type_name rec_fields
    | Expr_binary_op { binop_left; binop_operator; binop_right } ->
        let typed_binop_left = check_expr ctx binop_left in
        let typed_binop_right = check_expr ctx binop_right in
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

and check_stmt ctx stmt : Context.t * typed_stmt =
  let new_ctx, typed_stmt =
    match stmt.stmt_desc with
    | Stmt_expr expr ->
        let typed_expr = check_expr ctx expr in
        (ctx, TStmt_expr typed_expr)
    | Stmt_var { var_name; var_type; var_value } ->
        if Context.mem_local ctx var_name then
          Context.error (Errors.Redeclared_identifier var_name) stmt.stmt_loc;
        let translated_type = translate_type ctx var_type in
        let typed_var_value = check_expr ctx var_value in
        unification stmt.stmt_loc translated_type typed_var_value.texpr_type;
        let ctx' = Context.add_local ctx var_name translated_type in
        ( ctx',
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
        | None -> Context.error Errors.Return_outside_function stmt.stmt_loc);
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
  let ctx_with_params =
    List.fold_left
      (fun ctx (name, ty) -> Context.add_local ctx name ty)
      ctx translated_params
  in
  let return_type = translate_type ctx def.def_return_type in
  let ctx_with_params_and_return_type =
    Context.set_return_type ctx_with_params (Some return_type)
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
  match Context.find_type ctx record.rec_name with
  | Some (Types.TRec (rec_name, fields_ref)) ->
      (* check fields *)
      let check_field field =
        let field_type = translate_type ctx field.field_type in
        (field.field_name, field_type)
      in
      fields_ref := List.map check_field record.rec_fields;
      (* check methods  *)
      let record_type = Types.TRec (rec_name, fields_ref) in
      let ctx_with_self = Context.add_local ctx "self" record_type in
      let check_method method_def = check_def ctx_with_self method_def in
      let typed_methods = List.map check_method record.rec_methods in
      {
        trec_name = record.rec_name;
        trec_fields = !fields_ref;
        trec_methods = typed_methods;
        trec_loc = record.rec_loc;
      }
  | _ -> failwith "Unreachable: record type not found in context"

and check_toplevel ctx toplevel : Context.t * typed_toplevel =
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
  if Context.mem_local ctx def.def_name then
    Context.error (Errors.Redeclared_identifier def.def_name) def.def_loc;
  let translated_params = List.map (translate_param ctx) def.def_params in
  let param_types = List.map snd translated_params in
  let return_type = translate_type ctx def.def_return_type in
  let def_type = Types.TDef (param_types, return_type) in
  Context.add_local ctx def.def_name def_type

and load_extern ctx extern =
  Debug.trace_log "%s: loading: extern %s\n"
    (Location.show extern.extern_loc)
    extern.extern_name;
  if Context.mem_local ctx extern.extern_name then
    Context.error (Errors.Redeclared_identifier extern.extern_name)
      extern.extern_loc;
  let ctx_with_type_params =
    List.fold_left
      (fun ctx name -> Context.add_type ctx name (Types.TParam name))
      ctx extern.extern_type_params
  in
  let param_types =
    List.map (translate_type ctx_with_type_params) extern.extern_params
  in
  let return_type =
    translate_type ctx_with_type_params extern.extern_return_type
  in
  let extern_type = Types.TDef (param_types, return_type) in
  Context.add_local ctx extern.extern_name extern_type

and load_record ctx record =
  Debug.trace_log "%s: loading: rec %s\n"
    (Location.show record.rec_loc)
    record.rec_name;
  if Context.mem_type ctx record.rec_name then
    Context.error (Errors.Redeclared_identifier record.rec_name) record.rec_loc;
  let record_type = Types.TRec (record.rec_name, ref []) in
  let ctx_with_type = Context.add_type ctx record.rec_name record_type in
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
  let context_with_methods =
    List.fold_left
      (fun ctx (name, ty) -> Context.add_method ctx record.rec_name name ty)
      ctx_with_type
      (List.map load_method record.rec_methods)
  in
  context_with_methods

and load_toplevel ctx = function
  | Toplevel_def def -> load_def ctx def
  | Toplevel_extern extern -> load_extern ctx extern
  | Toplevel_rec record -> load_record ctx record

let check entry parsed_module =
  let initial_ctx = Context.create () in
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
  if not (Context.mem_local ctx_with_types entry) then
    Context.error (Errors.Entry_not_found entry) Location.none;
  let _ctx, typed_toplevels =
    List.fold_left_map check_toplevel ctx_with_types ordered_toplevels
  in
  let typed_module =
    {
      tmodule_name = parsed_module.module_name;
      tmodule_toplevels = typed_toplevels;
    }
  in
  if !Context.errors = [] then Ok typed_module
  else Error (List.rev !Context.errors)
