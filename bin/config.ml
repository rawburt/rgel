type t = {
  input_file : string;
  output_file : string;
  entry : string;
  quickjs : bool;
  runtimejs : string;
  trace : bool;
}

let default =
  {
    input_file = "";
    output_file = "out.js";
    entry = "main";
    quickjs = false;
    runtimejs = "./support/dist/bundle.js";
    trace = false;
  }

let parse () =
  let usage_msg = "rgel <file> -o <output-file>" in
  let input_files = ref [] in
  let output_file = ref default.output_file in
  let entry = ref default.entry in
  let quickjs = ref default.quickjs in
  let runtimejs = ref default.runtimejs in
  let trace = ref default.trace in
  let anon_fun filename = input_files := filename :: !input_files in
  let speclist =
    [
      ( "-o",
        Arg.Set_string output_file,
        Printf.sprintf "Specify output file (default: %s)" default.output_file
      );
      ( "-entry",
        Arg.Set_string entry,
        Printf.sprintf "Specify entry function (default: %s)" default.entry );
      ("-quickjs", Arg.Set quickjs, "Use QuickJS runtime");
      ( "-runtimejs",
        Arg.Set_string runtimejs,
        Printf.sprintf "Specify runtime JS file (default: %s)" default.runtimejs
      );
      ("-trace", Arg.Set trace, "Enable trace output");
    ]
  in
  Arg.parse speclist anon_fun usage_msg;
  if List.length !input_files <> 1 then (
    print_endline "No input file provided.";
    exit 1);
  {
    input_file = List.hd !input_files;
    output_file = !output_file;
    entry = !entry;
    quickjs = !quickjs;
    runtimejs = !runtimejs;
    trace = !trace;
  }
