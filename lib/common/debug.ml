let trace = ref false

let trace_log fmt =
  if !trace then Printf.fprintf stderr fmt else Printf.ifprintf stderr fmt
