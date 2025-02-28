open Vec

module type Matr_sig = sig
  type t
  type vec3

  val identity : unit -> t
  val create : float -> float -> float -> float
              -> float -> float -> float -> float
              -> float -> float -> float -> float
              -> float -> float -> float -> float -> t
  val get : t -> int -> int -> float
  val translate : vec3 -> t
  val scale : vec3 -> t
  val rotate : vec3 -> float -> t
  val rotate_x : float -> t
  val rotate_y : float -> t
  val rotate_z : float -> t
  val view : vec3 -> vec3 -> vec3 -> t
  val frustum : float -> float -> float -> float -> float -> float -> t
  val ortho : float -> float -> float -> float -> float -> float -> t
  val mult : t -> t -> t
  val mult_vec : t -> vec3 -> vec3
  val transform_normal : t -> vec3 -> vec3
  val point_transform : t -> vec3 -> vec3
  val vector_transform : t -> vec3 -> vec3
  val lerp : t -> t -> float -> t
  val of_bigarray : t -> (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
end

module Matr : Matr_sig with type vec3 = Vec3.t = struct
  type vec3 = Vec3.t
  type t = {
    m : float array array;
    inv_a : float array array;
    mutable is_inverse_evaluated : bool;
  }

  let create a00 a01 a02 a03 a10 a11 a12 a13 a20 a21 a22 a23 a30 a31 a32 a33 =
    {
      m = [|
        [| a00; a01; a02; a03 |];
        [| a10; a11; a12; a13 |];
        [| a20; a21; a22; a23 |];
        [| a30; a31; a32; a33 |];
      |];
      inv_a = Array.make_matrix 4 4 0.0;
      is_inverse_evaluated = false;
    }

  let get matrix i j = matrix.m.(i).(j)

  let identity () = create
    1. 0. 0. 0.
    0. 1. 0. 0.
    0. 0. 1. 0.
    0. 0. 0. 1.

  let translate v =
    let x = Vec3.x v and y = Vec3.y v and z = Vec3.z v in
    create
      1. 0. 0. 0.
      0. 1. 0. 0.
      0. 0. 1. 0.
      x y z 1.

  let scale v =
    let x = Vec3.x v and y = Vec3.y v and z = Vec3.z v in
    create
      x 0. 0. 0.
      0. y 0. 0.
      0. 0. z 0.
      0. 0. 0. 1.

  let determ3x3 a11 a12 a13 a21 a22 a23 a31 a32 a33 =
    a11 *. a22 *. a33
    -. a11 *. a23 *. a32
    -. a12 *. a21 *. a33
    +. a12 *. a23 *. a31
    +. a13 *. a21 *. a32
    -. a13 *. a22 *. a31

  let determ m =
    let m = m.m in
    let d0 = determ3x3 m.(1).(1) m.(1).(2) m.(1).(3)
                         m.(2).(1) m.(2).(2) m.(2).(3)
                         m.(3).(1) m.(3).(2) m.(3).(3) in
    let d1 = determ3x3 m.(1).(0) m.(1).(2) m.(1).(3)
                         m.(2).(0) m.(2).(2) m.(2).(3)
                         m.(3).(0) m.(3).(2) m.(3).(3) in
    let d2 = determ3x3 m.(1).(0) m.(1).(1) m.(1).(3)
                         m.(2).(0) m.(2).(1) m.(2).(3)
                         m.(3).(0) m.(3).(1) m.(3).(3) in
    let d3 = determ3x3 m.(1).(0) m.(1).(1) m.(1).(2)
                         m.(2).(0) m.(2).(1) m.(2).(2)
                         m.(3).(0) m.(3).(1) m.(3).(2) in
    m.(0).(0) *. d0 -. m.(0).(1) *. d1 +. m.(0).(2) *. d2 -. m.(0).(3) *. d3

  let evaluate_inverse matrix =
    if not matrix.is_inverse_evaluated then
      let det = determ matrix in
      let m = matrix.m in
      let inv = matrix.inv_a in
      let (/.) x y = x /. y in

      inv.(0).(0) <- determ3x3 m.(1).(1) m.(1).(2) m.(1).(3) m.(2).(1) m.(2).(2) m.(2).(3) m.(3).(1) m.(3).(2) m.(3).(3) /. det;
      inv.(1).(0) <- -. determ3x3 m.(1).(0) m.(1).(2) m.(1).(3) m.(2).(0) m.(2).(2) m.(2).(3) m.(3).(0) m.(3).(2) m.(3).(3) /. det;
      inv.(2).(0) <- determ3x3 m.(1).(0) m.(1).(1) m.(1).(3) m.(2).(0) m.(2).(1) m.(2).(3) m.(3).(0) m.(3).(1) m.(3).(3) /. det;
      inv.(3).(0) <- -. determ3x3 m.(1).(0) m.(1).(1) m.(1).(2) m.(2).(0) m.(2).(1) m.(2).(2) m.(3).(0) m.(3).(1) m.(3).(2) /. det;

      inv.(0).(1) <- -. determ3x3 m.(0).(1) m.(0).(2) m.(0).(3) m.(2).(1) m.(2).(2) m.(2).(3) m.(3).(1) m.(3).(2) m.(3).(3) /. det;
      inv.(1).(1) <- determ3x3 m.(0).(0) m.(0).(2) m.(0).(3) m.(2).(0) m.(2).(2) m.(2).(3) m.(3).(0) m.(3).(2) m.(3).(3) /. det;
      inv.(2).(1) <- -. determ3x3 m.(0).(0) m.(0).(1) m.(0).(3) m.(2).(0) m.(2).(1) m.(2).(3) m.(3).(0) m.(3).(1) m.(3).(3) /. det;
      inv.(3).(1) <- determ3x3 m.(0).(0) m.(0).(1) m.(0).(2) m.(2).(0) m.(2).(1) m.(2).(2) m.(3).(0) m.(3).(1) m.(3).(2) /. det;

      inv.(0).(2) <- determ3x3 m.(0).(1) m.(0).(2) m.(0).(3) m.(1).(1) m.(1).(2) m.(1).(3) m.(3).(1) m.(3).(2) m.(3).(3) /. det;
      inv.(1).(2) <- -. determ3x3 m.(0).(0) m.(0).(2) m.(0).(3) m.(1).(0) m.(1).(2) m.(1).(3) m.(3).(0) m.(3).(2) m.(3).(3) /. det;
      inv.(2).(2) <- determ3x3 m.(0).(0) m.(0).(1) m.(0).(3) m.(1).(0) m.(1).(1) m.(1).(3) m.(3).(0) m.(3).(1) m.(3).(3) /. det;
      inv.(3).(2) <- -. determ3x3 m.(0).(0) m.(0).(1) m.(0).(2) m.(1).(0) m.(1).(1) m.(1).(2) m.(3).(0) m.(3).(1) m.(3).(2) /. det;

      inv.(0).(3) <- -. determ3x3 m.(0).(1) m.(0).(2) m.(0).(3) m.(1).(1) m.(1).(2) m.(1).(3) m.(2).(1) m.(2).(2) m.(2).(3) /. det;
      inv.(1).(3) <- determ3x3 m.(0).(0) m.(0).(2) m.(0).(3) m.(1).(0) m.(1).(2) m.(1).(3) m.(2).(0) m.(2).(2) m.(2).(3) /. det;
      inv.(2).(3) <- -. determ3x3 m.(0).(0) m.(0).(1) m.(0).(3) m.(1).(0) m.(1).(1) m.(1).(3) m.(2).(0) m.(2).(1) m.(2).(3) /. det;
      inv.(3).(3) <- determ3x3 m.(0).(0) m.(0).(1) m.(0).(2) m.(1).(0) m.(1).(1) m.(1).(2) m.(2).(0) m.(2).(1) m.(2).(2) /. det;

      matrix.is_inverse_evaluated <- true

  let transform_normal matrix normal =
    evaluate_inverse matrix;
    let x = Vec3.x normal *. matrix.inv_a.(0).(0) +. Vec3.y normal *. matrix.inv_a.(0).(1) +. Vec3.z normal *. matrix.inv_a.(0).(2) in
    let y = Vec3.x normal *. matrix.inv_a.(1).(0) +. Vec3.y normal *. matrix.inv_a.(1).(1) +. Vec3.z normal *. matrix.inv_a.(1).(2) in
    let z = Vec3.x normal *. matrix.inv_a.(2).(0) +. Vec3.y normal *. matrix.inv_a.(2).(1) +. Vec3.z normal *. matrix.inv_a.(2).(2) in
    (x, y, z)

  let point_transform matrix v =
    let open Vec3 in
    let x = x v and y = y v and z = z v in
    let m = matrix.m in
    (
      (x *. m.(0).(0) +. y *. m.(1).(0) +. z *. m.(2).(0) +. m.(3).(0)),
      (x *. m.(0).(1) +. y *. m.(1).(1) +. z *. m.(2).(1) +. m.(3).(1)),
      (x *. m.(0).(2) +. y *. m.(1).(2) +. z *. m.(2).(2) +. m.(3).(2))
    )

  let vector_transform matrix v =
    let open Vec3 in
    let x = x v and y = y v and z = z v in
    let m = matrix.m in
    (
      (x *. m.(0).(0) +. y *. m.(1).(0) +. z *. m.(2).(0)),
      (x *. m.(0).(1) +. y *. m.(1).(1) +. z *. m.(2).(1)),
      (x *. m.(0).(2) +. y *. m.(1).(2) +. z *. m.(2).(2))
    )

  let mult_vec matrix v =
    let open Vec3 in
    let x = x v and y = y v and z = z v in
    let m = matrix.m in
    let w = x *. m.(0).(3) +. y *. m.(1).(3) +. z *. m.(2).(3) +. m.(3).(3) in
    let x' = (x *. m.(0).(0) +. y *. m.(1).(0) +. z *. m.(2).(0) +. m.(3).(0)) /. w in
    let y' = (x *. m.(0).(1) +. y *. m.(1).(1) +. z *. m.(2).(1) +. m.(3).(1)) /. w in
    let z' = (x *. m.(0).(2) +. y *. m.(1).(2) +. z *. m.(2).(2) +. m.(3).(2)) /. w in
    (x', y', z')

  let mult a b =
    let res = Array.make_matrix 4 4 0.0 in
    for i = 0 to 3 do
      for j = 0 to 3 do
        res.(i).(j) <- 0.0;
        for k = 0 to 3 do
          res.(i).(j) <- res.(i).(j) +. a.m.(i).(k) *. b.m.(k).(j)
        done
      done
    done;
    { m = res; inv_a = Array.make_matrix 4 4 0.0; is_inverse_evaluated = false }

  let rotate axis angle_deg =
    let rad = Float.pi /. 180.0 *. angle_deg in
    let c = cos rad and s = sin rad in
    let axis = Vec3.normalize axis in
    let x = Vec3.x axis and y = Vec3.y axis and z = Vec3.z axis in
    let t = 1.0 -. c in
    create
      (c +. x *. x *. t) (x *. y *. t +. z *. s) (x *. z *. t -. y *. s) 0.0
      (x *. y *. t -. z *. s) (c +. y *. y *. t) (y *. z *. t +. x *. s) 0.0
      (x *. z *. t +. y *. s) (y *. z *. t -. x *. s) (c +. z *. z *. t) 0.0
      0.0 0.0 0.0 1.0

  let rotate_x angle_deg =
    let rad = Float.pi /. 180.0 *. angle_deg in
    let c = cos rad and s = sin rad in
    create
      1.0 0.0 0.0 0.0
      0.0 c s 0.0
      0.0 (-.s) c 0.0
      0.0 0.0 0.0 1.0

  let rotate_y angle_deg =
    let rad = Float.pi /. 180.0 *. angle_deg in
    let c = cos rad and s = sin rad in
    create
      c 0.0 (-.s) 0.0
      0.0 1.0 0.0 0.0
      s 0.0 c 0.0
      0.0 0.0 0.0 1.0

  let rotate_z angle_deg =
    let rad = Float.pi /. 180.0 *. angle_deg in
    let c = cos rad and s = sin rad in
    create
      c s 0.0 0.0
      (-.s) c 0.0 0.0
      0.0 0.0 1.0 0.0
      0.0 0.0 0.0 1.0

  let view loc at up =
    let d = Vec3.normalize (Vec3.sub at loc) in
    let r = Vec3.normalize (Vec3.cross d up) in
    let u = Vec3.cross r d in
    let t = Vec3.neg loc in
    create
      (Vec3.x r) (Vec3.x u) (-.(Vec3.x d)) 0.0
      (Vec3.y r) (Vec3.y u) (-.(Vec3.y d)) 0.0
      (Vec3.z r) (Vec3.z u) (-.(Vec3.z d)) 0.0
      (Vec3.dot t r) (Vec3.dot t u) (Vec3.dot t d) 1.0

  let frustum l r b t n f =
    let two_n = 2.0 *. n in
    let rml = r -. l in
    let tmb = t -. b in
    let fmn = f -. n in
    create
      (two_n /. rml) 0.0 0.0 0.0
      0.0 (two_n /. tmb) 0.0 0.0
      ((r +. l) /. rml) ((t +. b) /. tmb) (-.(f +. n) /. fmn) (-1.0)
      0.0 0.0 (-.(two_n *. f) /. fmn) 0.0

  let ortho l r b t n f =
    let rml = r -. l in
    let tmb = t -. b in
    let fmn = f -. n in
    create
      (2.0 /. rml) 0.0 0.0 0.0
      0.0 (2.0 /. tmb) 0.0 0.0
      0.0 0.0 (-2.0 /. fmn) 0.0
      (-.(r +. l) /. rml) (-.(t +. b) /. tmb) (-.(n +. f) /. fmn) 1.0

  let lerp start end_ t =
    let t' = 1.0 -. t in
    let m00 = end_.m.(0).(0) *. t +. start.m.(0).(0) *. t' in
    let m01 = end_.m.(0).(1) *. t +. start.m.(0).(1) *. t' in
    let m02 = end_.m.(0).(2) *. t +. start.m.(0).(2) *. t' in
    let m03 = end_.m.(0).(3) *. t +. start.m.(0).(3) *. t' in
    let m10 = end_.m.(1).(0) *. t +. start.m.(1).(0) *. t' in
    let m11 = end_.m.(1).(1) *. t +. start.m.(1).(1) *. t' in
    let m12 = end_.m.(1).(2) *. t +. start.m.(1).(2) *. t' in
    let m13 = end_.m.(1).(3) *. t +. start.m.(1).(3) *. t' in
    let m20 = end_.m.(2).(0) *. t +. start.m.(2).(0) *. t' in
    let m21 = end_.m.(2).(1) *. t +. start.m.(2).(1) *. t' in
    let m22 = end_.m.(2).(2) *. t +. start.m.(2).(2) *. t' in
    let m23 = end_.m.(2).(3) *. t +. start.m.(2).(3) *. t' in
    let m30 = end_.m.(3).(0) *. t +. start.m.(3).(0) *. t' in
    let m31 = end_.m.(3).(1) *. t +. start.m.(3).(1) *. t' in
    let m32 = end_.m.(3).(2) *. t +. start.m.(3).(2) *. t' in
    let m33 = end_.m.(3).(3) *. t +. start.m.(3).(3) *. t' in
    create
      m00 m01 m02 m03
      m10 m11 m12 m13
      m20 m21 m22 m23
      m30 m31 m32 m33

  let of_bigarray matrix =
    let open Bigarray in
    let arr = Array1.create Float32 C_layout 16 in
    for i = 0 to 3 do
      for j = 0 to 3 do
        arr.{i * 4 + j} <- matrix.m.(i).(j)
      done
    done;
    arr
end