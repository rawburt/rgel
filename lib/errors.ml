exception CompilerError of string * Location.t

let syntax_error msg loc = raise (CompilerError ("syntax error: " ^ msg, loc))
