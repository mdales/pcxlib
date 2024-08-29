type t

type encoding_t =
| None
| RLE
| Unknown

type palette_mode_t =
| Unknown
| Colour
| Grayscale

val load: string -> (t, string) result

val dimensions: t -> (int * int)

val encoding: t -> encoding_t

val depth: t -> int

val planes: t -> int

val dpi: t -> (int * int)

val palette_mode: t -> palette_mode_t
