type t = { loc_start : Lexing.position; loc_end : Lexing.position }

let make_loc (start_pos, end_pos) = { loc_start = start_pos; loc_end = end_pos }

let string_of_loc t =
  Printf.sprintf "%s:%d:%d-%d:%d" t.loc_start.Lexing.pos_fname
    t.loc_start.Lexing.pos_lnum
    (t.loc_start.Lexing.pos_cnum - t.loc_start.Lexing.pos_bol + 1)
    t.loc_end.Lexing.pos_lnum
    (t.loc_end.Lexing.pos_cnum - t.loc_end.Lexing.pos_bol + 1)

let show t = string_of_loc t
let pp fmt t = Format.fprintf fmt "%s" (string_of_loc t)
