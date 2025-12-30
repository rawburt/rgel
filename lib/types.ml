type t =
  | TUnit
  | TBool
  | TInt
  | TStr
  | TDef of t list * t
  | TVar of t option ref

let show ty =
  let tbl = Hashtbl.create 16 in
  let counter = ref 0 in
  let rec show_aux = function
    | TUnit -> "unit"
    | TBool -> "bool"
    | TInt -> "int"
    | TStr -> "string"
    | TDef (arg_types, return_type) ->
        let args_str = arg_types |> List.map show_aux |> String.concat " -> " in
        Printf.sprintf "(%s) -> %s" args_str (show_aux return_type)
    | TVar { contents = Some t } -> show_aux t
    | TVar ({ contents = None } as r) -> (
        match Hashtbl.find_opt tbl r with
        | Some n -> Printf.sprintf "$%d" n
        | None ->
            incr counter;
            let n = !counter in
            Hashtbl.add tbl r n;
            Printf.sprintf "$%d" n)
  in
  show_aux ty

let rec final_ty ty =
  match ty with
  | TVar ({ contents = Some t } as r) ->
      let t' = final_ty t in
      r := Some t';
      t'
  | _ -> ty

let rec unify t1 t2 =
  let t1' = final_ty t1 in
  let t2' = final_ty t2 in
  match (t1', t2') with
  | TVar ({ contents = None } as r1), _ ->
      r1 := Some t2';
      true
  | _, TVar ({ contents = None } as r2) ->
      r2 := Some t1';
      true
  | TBool, TBool -> true
  | TInt, TInt -> true
  | TStr, TStr -> true
  | TDef (args1, ret1), TDef (args2, ret2) ->
      List.length args1 = List.length args2
      && List.for_all2 unify args1 args2
      && unify ret1 ret2
  | _ -> false
