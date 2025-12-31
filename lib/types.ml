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

let rec final_ty ty =
  match ty with
  | TVar (({ contents = Some t } as r), _) ->
      let t' = final_ty t in
      r := Some t';
      t'
  | _ -> ty

let rec unify t1 t2 =
  let t1' = final_ty t1 in
  let t2' = final_ty t2 in
  match (t1', t2') with
  | TVar (({ contents = None } as r1), _), _ ->
      r1 := Some t2';
      true
  | _, TVar (({ contents = None } as r2), _) ->
      r2 := Some t1';
      true
  | TBool, TBool -> true
  | TInt, TInt -> true
  | TStr, TStr -> true
  | TDef (args1, ret1), TDef (args2, ret2) ->
      List.length args1 = List.length args2
      && List.for_all2 unify args1 args2
      && unify ret1 ret2
  | TRec (name1, fields1), TRec (name2, fields2) when name1 = name2 ->
      if List.length !fields1 = List.length !fields2 then
        let sorted1 =
          List.sort (fun (n1, _) (n2, _) -> String.compare n1 n2) !fields1
        in
        let sorted2 =
          List.sort (fun (n1, _) (n2, _) -> String.compare n1 n2) !fields2
        in
        List.for_all2
          (fun (field_name1, field_type1) (field_name2, field_type2) ->
            field_name1 = field_name2 && unify field_type1 field_type2)
          sorted1 sorted2
      else false
  | _ -> false
