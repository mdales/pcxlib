

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
  data : int array;
  palette : (int * int * int) array option;
}

type encoding_t =
| None
| RLE
| Unknown

type palette_mode_t =
| Unknown
| Colour
| Grayscale

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

let read_uncompressed_data _ic _header =
  Result.error "Uncompressed data read not implemented yet"

let read_rle_data ic header =
  match header.bits_per_plane, header.planes with
  | 8, 1 -> (
    let width = (header.max_x + 1) - header.min_x
    and height = (header.max_y + 1) - header.min_y in

    In_channel.seek ic 128L;

    let result = Array.init (width * height) (fun _ -> 0) in

    let rec loop write_idx =
      let v = input_char ic in

      let data_val, rep_count = match ((int_of_char v) land 0xC0) with
      | 0xC0 -> input_char ic, ((int_of_char v) land 0x3F)
      | _ -> v, 1
      in

      for i = 0 to (rep_count - 1) do
        result.(i + write_idx) <- int_of_char data_val
      done;

      match ((write_idx + rep_count) >= width * height) with
      | true -> ()
      | false -> loop (write_idx + rep_count)
    in loop 0;

    Result.ok result
  )
  | d, p -> Result.error (Printf.sprintf "RLE decoding for depth %d and planes %d not yet implemeted" d p)

let read_data ic header =
  match header.encoding with
  | 0 -> read_uncompressed_data ic header
  | 1 -> read_rle_data ic header
  | _ -> Result.error "Unknown encoding type"

let read_palette ic header : ((int * int * int) array, string) result =
match header.bits_per_plane, header.planes with
  | 8, 1 -> (
    let filelength = In_channel.length ic in
    let palette_offset = Int64.sub filelength 768L in
    In_channel.seek ic palette_offset;
    Result.ok (Array.init 256 (fun _ ->
      (
        int_of_char (input_char ic),
        int_of_char (input_char ic),
        int_of_char (input_char ic)
      )
    )
  ))
  | d, p -> Result.error (Printf.sprintf "palette decoding for depth %d and planes %d not yet implemeted" d p)


let load filename =
  In_channel.with_open_bin filename (fun ic ->
    read_header ic >>= fun header ->
    read_data ic header >>= fun data ->
    read_palette ic header >>= fun palette ->
    match header.fixed with
    | 0xA -> Result.ok {header ; data; palette = Some palette }
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

let dpi t =
  (t.header.horizontal_dpi, t.header.vertical_dpi)

let palette_mode t =
  match t.header.palette_mode with
  | 1 -> Colour
  | 2 -> Grayscale
  | _ -> Unknown

let read_pixel t x y =
  t.data.((y * t.header.scan_line_length) + x)
