open Parsed_ast
open Type_common

let translate_param env param =
  let param_type = translate_type env param.param_type in
  (param.param_name, param_type)

let load_def env def =
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

let load_extern env extern =
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

let load_record env record =
  Debug.trace_log "%s: loading: rec %s\n"
    (Location.show record.rec_loc)
    record.rec_name;
  if Env.mem_type env record.rec_name then
    Env.error (Errors.Redeclared_identifier record.rec_name) record.rec_loc;
  let record_type = Types.TRec record.rec_name in
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

let load_toplevel env = function
  | Toplevel_def def -> load_def env def
  | Toplevel_extern extern -> load_extern env extern
  | Toplevel_rec record -> load_record env record

let load_module entry parsed_module =
  let env = Env.create () in
  let top = ordered_toplevels parsed_module.module_toplevels in
  let env_with_types = List.fold_left load_toplevel env top in
  if not (Env.mem_local env_with_types entry) then
    Env.error (Errors.Entry_not_found entry) Location.none;
  if Env.get_errors () <> [] then Error (Env.get_errors ())
  else Ok env_with_types
