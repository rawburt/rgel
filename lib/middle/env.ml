module StringMap = Map.Make (String)

type t = {
  types : Types.t StringMap.t;
  locals : Types.t StringMap.t;
  records : (string * Types.t) list StringMap.t;
  methods : Types.t StringMap.t StringMap.t;
  return_type : Types.t option;
}

let errors = ref []

let create () =
  errors := [];
  {
    types = Types.primitives |> StringMap.of_list;
    locals = StringMap.empty;
    records = StringMap.empty;
    methods = StringMap.empty;
    return_type = None;
  }

let error error loc = errors := Errors.Type_error (error, loc) :: !errors
let get_errors () = !errors
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

let add_type ctx name ty = { ctx with types = StringMap.add name ty ctx.types }

let add_record ctx name fields =
  let updated_records = StringMap.add name fields ctx.records in
  { ctx with records = updated_records }

let find_record ctx name = StringMap.find_opt name ctx.records

let add_method ctx rec_name method_name ty =
  let method_map =
    match find_methods ctx rec_name with
    | Some map -> map
    | None -> StringMap.empty
  in
  let updated_method_map = StringMap.add method_name ty method_map in
  let updated_methods = StringMap.add rec_name updated_method_map ctx.methods in
  { ctx with methods = updated_methods }

let set_return_type ctx return_type =
  { ctx with return_type = Some return_type }

let get_return_type ctx = ctx.return_type
