open Pcxlib

let usage_msg = "pcxinfo -f <pcx file>"
let args = ref []
let pcx_filename = ref ""
let anon_fun arg = args := !args @ [arg]
let speclist = [
  ("-f", Arg.Set_string pcx_filename, "PCX image file path");
]

let display_header_info img =
  let w, h = Pcx.dimensions img in
  Printf.printf "Dimensions: %d x %d\n" w h;
  Printf.printf "Depth:      %d\n" (Pcx.depth img);
  Printf.printf "Planes:     %d\n" (Pcx.planes img);
  let encoding = match (Pcx.encoding img) with
  | None -> "none"
  | RLE -> "run length encoding"
  | Unknown -> "unknown"
  in
  Printf.printf "Encoding:   %s\n" encoding

let () =
  Arg.parse speclist anon_fun usage_msg;

  match !pcx_filename with
  | "" -> Printf.printf "No filename provided\n"
  | filename -> (
    match Pcx.load filename with
    | Error desc -> Printf.printf "Error loading: %s\n" desc
    | Ok f -> display_header_info f
  )
