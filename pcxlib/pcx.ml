

type header = {
  fixed : int;
  version : int;
  encoding : int;
  bits_per_plane : int;
  min_x : int;
  min_y : int;
  max_x : int;
  max_y : int;
  horizontal_dpi : int;
  vertical_dpi : int;
  ega_palette : int list;
  reserved : int;
  planes : int;
  scan_line_length : int;
  palette_mode : int;
  horizontal_resoluton : int;
  vertical_resolution : int;
}

type t = {
  header : header;
  palette : int list;
}

type encoding_t =
| None
| RLE
| Unknown

let (>>=) = Result.bind

let read_header ic =
  let header_buffer = Bytes.create 128 in
  try
      really_input ic header_buffer 0 (Bytes.length header_buffer);
      Result.ok  {
        fixed = Bytes.get_int8 header_buffer 0 ;
        version = Bytes.get_int8 header_buffer 1 ;
        encoding = Bytes.get_int8 header_buffer 2 ;
        bits_per_plane = Bytes.get_int8 header_buffer 3 ;
        min_x = Bytes.get_int16_le header_buffer 4;
        min_y = Bytes.get_int16_le header_buffer 6;
        max_x = Bytes.get_int16_le header_buffer 8;
        max_y = Bytes.get_int16_le header_buffer 10;
        horizontal_dpi = Bytes.get_int16_le header_buffer 12;
        vertical_dpi = Bytes.get_int16_le header_buffer 14;
        ega_palette = []; (* todo *)
        reserved = Bytes.get_int8 header_buffer 64;
        planes = Bytes.get_int8 header_buffer 65;
        scan_line_length = Bytes.get_int16_le header_buffer 66;
        palette_mode = Bytes.get_int16_le header_buffer 68;
        horizontal_resoluton = Bytes.get_int16_le header_buffer 70;
        vertical_resolution = Bytes.get_int16_le header_buffer 72;
      }
    with
    | Sys_error(reason) -> Result.error reason


let load filename =
  In_channel.with_open_bin filename (fun ic ->
    read_header ic >>= fun header ->
    match header.fixed with
    | 0xA -> Result.ok {header ; palette = []}
    | _ -> Result.error (Printf.sprintf "unexpected magic number 0x%x" header.fixed)
  )

let dimensions t =
  ((1 + t.header.max_x - t.header.min_x), (1 + t.header.max_y - t.header.min_y))

let encoding t =
  match t.header.encoding with
  | 0 -> None
  | 1 -> RLE
  | _ -> Unknown

let depth t =
  t.header.bits_per_plane

let planes t =
  t.header.planes
