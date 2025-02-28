open Vec
open Mat

module type Camera_sig = sig
  type t
  type vec3
  type matr

  val create : unit -> t
  val set_proj : t -> float -> float -> float -> t
  val resize : t -> int -> int -> t
  val set_loc_at_up : t -> vec3 -> vec3 -> vec3 -> t
  val rotate : t -> vec3 -> float -> t
  val move : t -> vec3 -> t
  val get_view : t -> matr
  val get_proj : t -> matr
  val get_vp : t -> matr
  val get_dir : t -> vec3
  val get_right : t -> vec3
end

module Camera : Camera_sig with type vec3 = Vec3.t and type matr = Matr.t = struct
  type vec3 = Vec3.t
  type matr = Matr.t
  type t = {
    loc : vec3;
    dir : vec3;
    up : vec3;
    right : vec3;
    at : vec3;
    proj_dist : float;
    far_clip : float;
    size : float;
    frame_w : int;
    frame_h : int;
    view : matr;
    proj : matr;
    vp : matr;
  }

  let update_proj camera =
    let ratio_x = camera.size /. 2.0 in
    let ratio_y = camera.size /. 2.0 in
    let ratio_x, ratio_y =
      if camera.frame_w >= camera.frame_h then
        (ratio_x *. (float_of_int camera.frame_w /. float_of_int camera.frame_h), ratio_y)
      else
        (ratio_x, ratio_y *. (float_of_int camera.frame_h /. float_of_int camera.frame_w))
    in
    let proj = Matr.frustum (-.ratio_x) ratio_x (-.ratio_y) ratio_y camera.proj_dist camera.far_clip in
    { camera with proj; vp = Matr.mult camera.view proj }

  let update_view camera =
    let view = Matr.view camera.loc camera.at camera.up in
    (* Extract the third row (z-axis) of the view matrix *)
    let dir = Vec3.neg (Matr.get view 2 0, Matr.get view 2 1, Matr.get view 2 2) in
    (* Extract the first row (x-axis) of the view matrix *)
    let right = Vec3.neg (Matr.get view 0 0, Matr.get view 0 1, Matr.get view 0 2) in
    { camera with view; dir; right; vp = Matr.mult view camera.proj }

  let create () =
    let loc = (0.0, 0.0, 5.0) in
    let dir = (0.0, 0.0, (-1.0)) in
    let up = (0.0, 1.0, 0.0) in
    let right = (1.0, 0.0, 0.0) in
    let at = (0.0, 0.0, 0.0) in
    let proj_dist = 0.1 in
    let far_clip = 1000.0 in
    let size = 0.1 in
    let frame_w = 30 in
    let frame_h = 30 in
    let view = Matr.translate (0.0, 0.0, (-3.0)) in
    let camera = { loc; dir; up; right; at; proj_dist; far_clip; size; frame_w; frame_h; view; proj = Matr.identity(); vp = Matr.identity() } in
    update_proj (camera)

  let set_proj camera new_size new_proj_dist new_far_clip =
    let camera = { camera with size = new_size; proj_dist = new_proj_dist; far_clip = new_far_clip } in
    update_proj camera

  let resize camera new_frame_w new_frame_h =
    let camera = { camera with frame_w = new_frame_w; frame_h = new_frame_h } in
    update_proj camera

  let set_loc_at_up camera loc at up =
    let camera = { camera with loc; at; up } in
    update_view camera

  let rotate camera axis angle_deg =
    let rot_matrix = Matr.rotate axis angle_deg in
    let at = Matr.point_transform rot_matrix (Vec3.sub camera.at camera.loc) |> Vec3.add camera.loc in
    let up = Matr.vector_transform rot_matrix camera.up in
    set_loc_at_up camera camera.loc at up

  let move camera direction =
    let loc = Vec3.add camera.loc direction in
    let at = Vec3.add camera.at direction in
    set_loc_at_up camera loc at camera.up

  let get_view camera = camera.view
  let get_proj camera = camera.proj
  let get_vp camera = camera.vp
  let get_dir camera = camera.dir
  let get_right camera = camera.right
end