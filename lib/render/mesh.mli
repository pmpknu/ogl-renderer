open Math.Vec

module Vertex : sig
  type t

  val create :
    position:Vec3.t ->
    normal:Vec3.t ->
    tex_coords:Vec2.t ->
    tangent:Vec3.t ->
    bitangent:Vec3.t ->
    bone_ids:int array ->
    weights:float array ->
    t

  val position : t -> Vec3.t
  val normal : t -> Vec3.t
  val tex_coords : t -> Vec2.t
  val tangent : t -> Vec3.t
  val bitangent : t -> Vec3.t
  val bone_ids : t -> int array
  val weights : t -> float array
end

(** Represents a texture with an ID, type, and file path. *)
module MeshTexture : sig
  type t = {
    id : int;
    type_ : string;
    path : string;
  }

  val create : id:int -> type_:string -> path:string -> t
  val id : t -> int
  val type_ : t -> string
  val path : t -> string
end

(** Represents a mesh composed of vertices, indices, and textures. *)
module Mesh : sig
  type t

  val create :
    vertices:Vertex.t array ->
    indices:int array ->
    textures:MeshTexture.t array ->
    t

  val draw : t -> int -> unit
end
