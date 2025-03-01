val bigarray_create :
  ('a, 'b) Bigarray.kind -> int -> ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t
(** Helper function to create a Bigarray of given kind and length *)

val get_int :
  ((int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t -> unit) ->
  int
(** Helper function to get an integer value from OpenGL *)

val check_shader_compilation : int -> unit
(** Function to check the compilation status of a shader *)

val check_program_linking : int -> unit
(** Function to check the linking status of a shader program *)

val create_shader : int -> string -> int
(** Function to create and compile a shader *)

val create_shader_program : string -> string -> int
(** Function to create and link a shader program from vertex and fragment source
*)

val use : int -> unit
(** Function to use the shader program *)

val set_uniform_bool : int -> string -> bool -> unit
(** Utility function to set a boolean uniform variable *)

val set_uniform_int : int -> string -> int -> unit
(** Utility function to set an integer uniform variable *)

val set_uniform_float : int -> string -> float -> unit
(** Utility function to set a float uniform variable *)
