{

open Grammar
open Lexing

let keywords = [
  ("true", TRUE);
  ("false", FALSE);
  ("def", DEF);
  ("do", DO);
  ("end", END);
  ("extern", EXTERN);
  ("rec", REC);
]

let kw_or_ident s = try List.assoc s keywords with Not_found -> IDENT s

let error msg lexbuf =
  let start_pos = lexbuf.lex_start_p in
  let current_pos = lexbuf.lex_curr_p in
  let loc = Location.make_loc (start_pos, current_pos) in
  Errors.throw_syntax_error msg loc

}

let ident = ['a'-'z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9']*
let tident = ['A'-'Z']['a'-'z' 'A'-'Z' '_' '0'-'9']*
let integer = ['0'-'9']+
let ws = [' ' '\t' '\r']+

rule token = parse
  | ws            { token lexbuf }
  | '#'           { comment lexbuf }
  | '\n'          { new_line lexbuf; token lexbuf }
  | '"'           { read_string (Buffer.create 16) lexbuf }
  | '('           { LPAREN }
  | ')'           { RPAREN }
  | '['           { LBRACKET }
  | ']'           { RBRACKET }
  | ','           { COMMA }
  | ':'           { COLON }
  | '='           { EQ }
  | '+'           { PLUS }
  | '-'           { MINUS }
  | '*'           { STAR }
  | '/'           { SLASH }
  | "=="          { EQEQ }
  | ident as i    { kw_or_ident i }
  | tident as t   { TIDENT t }
  | integer as i  { INT_LITERAL (int_of_string i) }
  | eof           { EOF }
  | _             { error (Errors.Unexpected_token (Lexing.lexeme lexbuf)) lexbuf }
and comment = parse
  | '\n'  { new_line lexbuf; token lexbuf }
  | _     { comment lexbuf }
  | eof   { EOF }
and read_string buf = parse
  | '"'           { STRING_LITERAL (Buffer.contents buf) }
  | [^ '"' '\\']+ { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
  | eof           { error Errors.String_not_terminated lexbuf }
  | _             { error Errors.Unexpected_string_item lexbuf }
