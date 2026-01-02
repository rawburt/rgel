type t =
  | TUnit
  | TBool
  | TInt
  | TStr
  | TDef of t list * t
  | TVar of t option ref * int
  | TParam of string
  | TRec of string * (string * t) list ref

let fresh_var =
  let counter = ref 0 in
  fun () ->
    let id = !counter in
    counter := id + 1;
    TVar (ref None, id)

let show ty =
  let rec show_aux = function
    | TUnit -> "unit"
    | TBool -> "bool"
    | TInt -> "int"
    | TStr -> "string"
    | TDef (arg_types, return_type) ->
        let args_str = arg_types |> List.map show_aux |> String.concat " -> " in
        Printf.sprintf "(%s) -> %s" args_str (show_aux return_type)
    | TVar ({ contents = Some t }, _) -> show_aux t
    | TVar ({ contents = None }, id) -> "$" ^ string_of_int id
    | TParam name -> name
    | TRec (name, fields) ->
        let field_strs =
          !fields
          |> List.map (fun (fname, ftype) ->
                 Printf.sprintf "%s: %s" fname (show_aux ftype))
        in
        let fields_str = String.concat ", " field_strs in
        Printf.sprintf "%s(%s)" name fields_str
  in
  show_aux ty

let pp fmt ty = Format.fprintf fmt "%s" (show ty)

let instantiate ty =
  let tbl = Hashtbl.create 8 in
  let rec aux ty =
    match ty with
    | TUnit -> TUnit
    | TBool -> TBool
    | TInt -> TInt
    | TStr -> TStr
    | TDef (arg_types, return_type) ->
        let new_args = List.map aux arg_types in
        let new_return = aux return_type in
        TDef (new_args, new_return)
    | TVar ({ contents = None }, id) -> TVar (ref None, id)
    | TVar ({ contents = Some t }, _) -> aux t
    | TParam name -> (
        match Hashtbl.find_opt tbl name with
        | Some tv -> tv
        | None ->
            let tv = fresh_var () in
            Hashtbl.add tbl name tv;
            tv)
    | TRec (name, fields) ->
        let new_fields =
          !fields |> List.map (fun (name, ty) -> (name, aux ty))
        in
        TRec (name, ref new_fields)
  in
  aux ty

let rec occurs id ty =
  match ty with
  | TVar ({ contents = None }, id1) -> id1 = id
  | TVar ({ contents = Some t }, _) -> occurs id t
  | TDef (params, ret) -> List.exists (occurs id) params || occurs id ret
  | TRec (_, fields_ref) ->
      List.exists (fun (_, field_ty) -> occurs id field_ty) !fields_ref
  | TBool | TInt | TStr | TUnit | TParam _ -> false

let rec unify t1 t2 =
  match (t1, t2) with
  | TVar ({ contents = Some t1' }, _), TVar ({ contents = Some t2' }, _) ->
      unify t1' t2'
  | TVar (({ contents = None } as r1), id1), _ ->
      if occurs id1 t2 then false
      else (
        r1 := Some t2;
        true)
  | _, TVar (({ contents = None } as r2), id2) ->
      if occurs id2 t1 then false
      else (
        r2 := Some t1;
        true)
  | TDef (args1, ret1), TDef (args2, ret2) ->
      List.length args1 = List.length args2
      && List.for_all2 unify args1 args2
      && unify ret1 ret2
  | TRec (name1, fields1), TRec (name2, fields2) ->
      name1 = name2
      && List.length !fields1 = List.length !fields2
      && List.for_all2
           (fun (n1, t1) (n2, t2) -> n1 = n2 && unify t1 t2)
           !fields1 !fields2
  | TBool, TBool | TInt, TInt | TStr, TStr -> true
  | TParam name1, TParam name2 when name1 = name2 -> true
  | _ -> false
