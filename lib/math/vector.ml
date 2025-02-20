module Vec3d : sig
  type t = private float * float * float

  val mk : float * float * float -> t
  val raw : t -> float * float * float
  val c1 : t -> float
  val c2 : t -> float
  val c3 : t -> float
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( ~- ) : t -> t
  val ( * ) : float -> t -> t
  val ( *! ) : int -> t -> t
  val ( / ) : t -> float -> t
  val ( /! ) : t -> int -> t
  val length_square : t -> float
  val unit : t -> t
  val dot : t -> t -> float
end = struct
  type t = float * float * float

  let mk v = v
  let raw v = v
  let c1 (v1, _, _) = v1
  let c2 (_, v2, _) = v2
  let c3 (_, _, v3) = v3

  let ( + ) (v11, v12, v13) (v21, v22, v23) =
    (v11 +. v21, v12 +. v22, v13 +. v23)

  let ( - ) (v11, v12, v13) (v21, v22, v23) =
    (v11 -. v21, v12 -. v22, v13 -. v23)

  let ( ~- ) (a, b, c) = (-.a, -.b, -.c)
  let ( * ) s (v1, v2, v3) = (s *. v1, s *. v2, s *. v3)
  let ( *! ) is v = float_of_int is * v
  let ( / ) (v1, v2, v3) s = (v1 /. s, v2 /. s, v3 /. s)
  let ( /! ) v is = v / float_of_int is
  let length_square (v1, v2, v3) = (v1 *. v1) +. (v2 *. v2) +. (v3 *. v3)
  let length v = sqrt (length_square v)
  let unit v = v / length v
  let dot (a1, a2, a3) (b1, b2, b3) = (a1 *. b1) +. (a2 *. b2) +. (a3 *. b3)

  (* ppx inline tests *)
end

