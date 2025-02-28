module Vector = struct
  (* A vector is represented as a 1D Owl ndarray *)
  type t = Owl.Dense.Ndarray.D.arr

  (** [vector n] returns a function that constructs an [n]-dimensional vector
      from a list of floats. For example:
      {[
        let vec2 = Vector.vector 2 in
        let v = vec2 [1.; 2.]
      ]}
  *)
  let vector n =
    fun lst ->
      if List.length lst <> n then
        failwith "Dimension mismatch in vector constructor"
      else
        Owl.Dense.Ndarray.D.of_array (Array.of_list lst) [| n |]

  (** [add v1 v2] returns the elementwise sum of vectors [v1] and [v2]. *)
  let add v1 v2 = Owl.Dense.Ndarray.D.add v1 v2

  (** [sub v1 v2] returns the elementwise difference of vectors [v1] and [v2]. *)
  let sub v1 v2 = Owl.Dense.Ndarray.D.sub v1 v2

  (** [neg v] returns the negation of vector [v]. *)
  let neg v = Owl.Dense.Ndarray.D.neg v

  (** [scalar_mul s v] multiplies vector [v] by scalar [s]. *)
  let scalar_mul s v = Owl.Dense.Ndarray.D.mul_scalar v s

  (** [dot v1 v2] returns the dot product of vectors [v1] and [v2]. *)
  let dot v1 v2 =
    Owl.Dense.Ndarray.D.(sum (mul v1 v2))

  (** [norm v] returns the Euclidean (L2) norm of vector [v]. *)
  let norm v = Owl.Dense.Ndarray.D.vecnorm v

  (** [mul v1 v2] returns the elementwise product of [v1] and [v2]. *)
  let mul v1 v2 = Owl.Dense.Ndarray.D.mul v1 v2
end

module Matrix = struct
  type t = Owl.Mat.mat

  (** [add a b] returns the sum of matrices [a] and [b]. *)
  let add a b = Owl.Mat.add a b

  (** [sub a b] returns the difference of matrices [a] and [b]. *)
  let sub a b = Owl.Mat.sub a b

  (** [scalar_mul a s] multiplies every element of matrix [a] by scalar [s]. *)
  let scalar_mul a s = Owl.Mat.scalar_mul a s

  (** [dot a b] returns the matrix–matrix product of [a] and [b]. *)
  let dot a b = Owl.Mat.dot a b

  (** [mul_vec m v] multiplies matrix [m] by column vector [v]. 
      Here [v] is represented as a [n × 1] matrix. *)
  let mul_vec m v = Owl.Mat.mul m v

  (** [identity n] returns the [n × n] identity matrix. *)
  let identity n = Owl.Mat.eye n

  (** [scaling sx sy sz] returns a 4×4 scaling transformation matrix. *)
  let scaling sx sy sz =
    Owl.Mat.of_array
      [| sx; 0.;  0.;  0.;
          0.; sy;  0.;  0.;
          0.;  0.; sz;  0.;
          0.;  0.;  0.;  1. |] 4 4

  (** [translation tx ty tz] returns a 4×4 translation transformation matrix. *)
  let translation tx ty tz =
    Owl.Mat.of_array
      [| 1.; 0.; 0.; tx;
          0.; 1.; 0.; ty;
          0.; 0.; 1.; tz;
          0.; 0.; 0.; 1. |] 4 4

  (** [rotation_x angle] returns a 4×4 matrix representing a rotation about the x-axis by [angle] radians. *)
  let rotation_x angle =
    let c = cos angle and s = sin angle in
    Owl.Mat.of_array
      [| 1.; 0.;  0.; 0.;
          0.;  c; -.s; 0.;
          0.;  s;  c; 0.;
          0.; 0.;  0.; 1. |] 4 4

  (** [rotation_y angle] returns a 4×4 matrix representing a rotation about the y-axis by [angle] radians. *)
  let rotation_y angle =
    let c = cos angle and s = sin angle in
    Owl.Mat.of_array
      [|  c; 0.; s; 0.;
          0.; 1.; 0.; 0.;
          -.s; 0.; c; 0.;
          0.; 0.; 0.; 1. |] 4 4

  (** [rotation_z angle] returns a 4×4 matrix representing a rotation about the z-axis by [angle] radians. *)
  let rotation_z angle =
    let c = cos angle and s = sin angle in
    Owl.Mat.of_array
      [| c; -.s; 0.; 0.;
          s;  c;  0.; 0.;
          0.; 0.;  1.; 0.;
          0.; 0.;  0.; 1. |] 4 4
  let rotation tx ty tz angle =
    let c = cos angle and s = sin angle and t = 1. -. cos angle in
    Owl.Mat.of_array
      [| t *. tx *. tx +. c; t *. tx *. ty +. tz *. s; t *. tx *. tz -. ty *. s; 0.;
          t *. tx *. ty -. tz *. s; t *. ty *. ty +. c; t *. ty *. tz +. tx *. s; 0.;
          t *. tx *. tz +. ty *. s; t *. ty *. tz -. tx *. s; t *. tz *. tz +. c; 0.;
          0.; 0.; 0.; 1. |] 4 4

  (** [combine a b] returns the product of matrices [a] and [b], which is equivalent to combining two transformations. *)
  let combine a b = dot a b

  (** [gl make uniform] returns the correct type for Tgl3.Gl.uniform_matrix4fv *)
  let gl m =
    let arr = Owl.Mat.to_array m in
    let ba = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout (Array.length arr) in
    Array.iteri (fun i v -> Bigarray.Array1.set ba i v) arr;
    ba

end
