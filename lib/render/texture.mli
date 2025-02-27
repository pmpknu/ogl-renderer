val load_image : string -> int * int * int * (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
val create_texture : int -> int -> int -> (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t -> int
val load_texture : string -> int
val bind : int -> unit