type t

val make_loc : Lexing.position * Lexing.position -> t
val string_of_loc : t -> string
val show : t -> string
val pp : Format.formatter -> t -> unit
