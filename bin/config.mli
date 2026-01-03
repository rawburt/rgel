type t = {
  input_file : string;
  output_file : string;
  entry : string;
  quickjs : bool;
  runtimejs : string;
  trace : bool;
}

val parse : unit -> t
