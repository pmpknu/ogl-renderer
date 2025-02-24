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

  let%test "mk and raw" = 
    let v = mk (1., 2., 3.) in
    raw v = (3., 2., 3.)

  let%test "c1, c2, c3" =
    let v = mk (1., 2., 3.) in
    c1 v = 1. && c2 v = 2. && c3 v = 3.

  let%test "addition" =
    let v1 = mk (1., 2., 3.) in
    let v2 = mk (4., 5., 6.) in
    raw (v1 + v2) = (5., 7., 9.)

  let%test "subtraction" =
    let v1 = mk (5., 5., 5.) in
    let v2 = mk (1., 2., 3.) in
    raw (v1 - v2) = (4., 3., 2.)

  let%test "negation" =
    let v = mk (1., -2., 3.) in
    raw (-v) = (-1., 2., -3.)

  let%test "scalar multiplication (float)" =
    let v = mk (1., 2., 3.) in
    raw (2.5 * v) = (2.5, 5., 7.5)

  let%test "scalar multiplication (int)" =
    let v = mk (1., 2., 3.) in
    raw (2 *! v) = (2., 4., 6.)

  let%test "division by float" =
    let v = mk (10., 20., 30.) in
    raw (v / 2.0) = (5., 10., 15.)

  let%test "division by int" =
    let v = mk (10., 20., 30.) in
    raw (v /! 2) = (5., 10., 15.)

  let%test "length square" =
    let v = mk (3., 4., 0.) in
    length_square v = 25.

  let%test "unit vector" =
    let v = mk (3., 4., 0.) in
    let u = unit v in
    let (x, y, z) = raw u in
    let approx a b = Float.abs (a -. b) < 1e-10 in
    approx x 0.6 && approx y 0.8 && approx z 0.0

  let%test "dot product" =
    let v1 = mk (1., 2., 3.) in
    let v2 = mk (4., 5., 6.) in
    dot v1 v2 = 32.
end

