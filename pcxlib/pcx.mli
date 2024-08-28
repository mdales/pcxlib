type t

type encoding_t =
| None
| RLE
| Unknown

val load: string -> (t, string) result

val dimensions: t -> (int * int)

val encoding: t -> encoding_t

val depth: t -> int

val planes: t -> int
