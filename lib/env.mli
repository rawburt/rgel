type t

val create : unit -> t
val find_type : t -> string -> Types.t option
val add_type : t -> string -> Types.t -> t
val mem_type : t -> string -> bool
val find_method : t -> string -> string -> Types.t option
val add_method : t -> string -> string -> Types.t -> t
val find_local : t -> string -> Types.t option
val mem_local : t -> string -> bool
val add_local : t -> string -> Types.t -> t
val set_return_type : t -> Types.t -> t
val get_return_type : t -> Types.t option
val error : Errors.type_error -> Location.t -> unit
val get_errors : unit -> Errors.error list
