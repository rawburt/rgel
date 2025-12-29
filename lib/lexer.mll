{

open Grammar
open Lexing

let keywords = [
  ("true", TRUE);
  ("false", FALSE);
  ("def", DEF);
  ("do", DO);
  ("end", END);
]

let kw_or_ident s = try List.assoc s keywords with Not_found -> IDENT s

let error msg lexbuf =
  let start_pos = lexbuf.lex_start_p in
  let current_pos = lexbuf.lex_curr_p in
  let loc = Location.make_loc (start_pos, current_pos) in
  Errors.syntax_error msg loc

}

let ident = ['a'-'z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9']*
let integer = ['0'-'9']+
let ws = [' ' '\t' '\r']+

rule token = parse
  | ws            { token lexbuf }
  | '#'           { comment lexbuf }
  | '\n'          { new_line lexbuf; token lexbuf }
  | '"'           { read_string (Buffer.create 16) lexbuf }
  | '('           { LPAREN }
  | ')'           { RPAREN }
  | ','           { COMMA }
  | ident as i    { kw_or_ident i }
  | integer as i  { INT_LITERAL (int_of_string i) }
  | eof           { EOF }
  | _             { error ("unexpected character: " ^ (Lexing.lexeme lexbuf)) lexbuf }
and comment = parse
  | '\n'  { new_line lexbuf; token lexbuf }
  | _     { comment lexbuf }
  | eof   { EOF }
and read_string buf = parse
  | '"'           { STRING_LITERAL (Buffer.contents buf) }
  | [^ '"' '\\']+ { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
  | eof           { error "string not terminated" lexbuf }
  | _             { error "unexpected string item" lexbuf }
