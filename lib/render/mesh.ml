open Tgl3
open Math.Vec

(** Vertex module *)
module Vertex = struct
  type t = {
    position : Vec3.t;
    normal : Vec3.t;
    tex_coords : Vec2.t;
    tangent : Vec3.t;
    bitangent : Vec3.t;
    bone_ids : int array;
    weights : float array;
  }

  let create ~position ~normal ~tex_coords ~tangent ~bitangent ~bone_ids
      ~weights =
    { position; normal; tex_coords; tangent; bitangent; bone_ids; weights }

  let position v = v.position
  let normal v = v.normal
  let tex_coords v = v.tex_coords
  let tangent v = v.tangent
  let bitangent v = v.bitangent
  let bone_ids v = v.bone_ids
  let weights v = v.weights
end

(** Texture module for ASSIMP, for Texture loading from file use texture.ml *)
module MeshTexture = struct
  type t = { id : int; type_ : string; path : string }

  let create ~id ~type_ ~path = { id; type_; path }
  let id t = t.id
  let type_ t = t.type_
  let path t = t.path
end

(** Mesh module *)
module Mesh = struct
  type t = {
    vertices : Vertex.t array;
    indices : int array;
    textures : MeshTexture.t array;
    vao : int;
    vbo : int;
    ebo : int;
  }

  (* Temperal for disable [@@warning "-69"] *)
  let _vertices m = m.vertices
  let _vbo m = m.vbo
  let _ebo m = m.ebo


  (** Set up the mesh by creating and configuring VAO, VBO, and EBO. *)
  let setup_mesh vertices indices =
    let vao = Shader.get_int (Tgl3.Gl.gen_vertex_arrays 1) in
    Tgl3.Gl.bind_vertex_array vao;

    (* Load vertices into VBO *)
    let vbo = Shader.get_int (Tgl3.Gl.gen_buffers 1) in
    Tgl3.Gl.bind_buffer Tgl3.Gl.array_buffer vbo;
    let vertex_data =
      Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout
        (Array.concat
          (Array.to_list
              (Array.map
                (fun v ->
                  [|
                    (* Position, Normal, TexCoords, Tangent, Bitangent *)
                    Vec3.x v.Vertex.position;
                    Vec3.y v.Vertex.position;
                    Vec3.z v.Vertex.position;
                    Vec3.x v.Vertex.normal;
                    Vec3.y v.Vertex.normal;
                    Vec3.z v.Vertex.normal;
                    Vec2.x v.Vertex.tex_coords;
                    Vec2.y v.Vertex.tex_coords;
                    Vec3.x v.Vertex.tangent;
                    Vec3.y v.Vertex.tangent;
                    Vec3.z v.Vertex.tangent;
                    Vec3.x v.Vertex.bitangent;
                    Vec3.y v.Vertex.bitangent;
                    Vec3.z v.Vertex.bitangent;
                    (* Store bone IDs as floats (temporary fix) *)
                    float_of_int v.Vertex.bone_ids.(0);
                    float_of_int v.Vertex.bone_ids.(1);
                    float_of_int v.Vertex.bone_ids.(2);
                    float_of_int v.Vertex.bone_ids.(3);
                    v.Vertex.weights.(0);
                    v.Vertex.weights.(1);
                    v.Vertex.weights.(2);
                    v.Vertex.weights.(3);
                  |])
                vertices)))
    in
    Gl.buffer_data Gl.array_buffer
      (Bigarray.Array1.size_in_bytes vertex_data)  (* Corrected size *)
      (Some vertex_data) Gl.static_draw;

    (* Load indices into EBO *)
    let ebo = Shader.get_int (Tgl3.Gl.gen_buffers 1) in
    Tgl3.Gl.bind_buffer Tgl3.Gl.element_array_buffer ebo;
    let index_data =
      Bigarray.Array1.of_array Bigarray.int32 Bigarray.c_layout
        (Array.map Int32.of_int indices)
    in
    Gl.buffer_data Gl.element_array_buffer
      (Bigarray.Array1.size_in_bytes index_data)  (* Corrected size *)
      (Some index_data) Gl.static_draw;

    (* Set vertex attribute pointers *)
    let stride = 22 * 4 in  (* 14 floats + 4 floats (bone IDs) + 4 floats (weights) *)
    (* Position *)
    Gl.enable_vertex_attrib_array 0;
    Gl.vertex_attrib_pointer 0 3 Gl.float false stride (`Offset 0);
    (* ... other attributes ... *)
    (* Bone IDs (as floats) *)
    Gl.enable_vertex_attrib_array 5;
    Gl.vertex_attrib_pointer 5 4 Gl.float false stride (`Offset (14 * 4));
    (* Weights *)
    Gl.enable_vertex_attrib_array 6;
    Gl.vertex_attrib_pointer 6 4 Gl.float false stride (`Offset (18 * 4));

    Gl.bind_vertex_array 0;
    (vao, vbo, ebo)

  (** Create a mesh from vertices, indices, and textures. *)
  let create ~vertices ~indices ~textures =
    let vao, vbo, ebo = setup_mesh vertices indices in
    { vertices; indices; textures; vao; vbo; ebo }

  (** Draw the mesh using the specified shader. *)
  let draw mesh shaderId =
    (* Bind textures *)
    let diffuse_nr = ref 1 in
    let specular_nr = ref 1 in
    let normal_nr = ref 1 in
    let height_nr = ref 1 in
    Array.iteri
      (fun i texture ->
        Gl.active_texture (Gl.texture0 + i);
        let number =
          match texture.MeshTexture.type_ with
          | "texture_diffuse" -> string_of_int !diffuse_nr
          | "texture_specular" -> string_of_int !specular_nr
          | "texture_normal" -> string_of_int !normal_nr
          | "texture_height" -> string_of_int !height_nr
          | _ -> ""
        in
        let name = texture.MeshTexture.type_ ^ number in
        Shader.set_uniform_int shaderId name i;
        Gl.bind_texture Gl.texture_2d texture.MeshTexture.id)
      mesh.textures;

    (* Draw mesh *)
    Gl.bind_vertex_array mesh.vao;
    Gl.draw_elements Gl.triangles
      (Array.length mesh.indices)
      Gl.unsigned_int (`Offset 0);
    Gl.bind_vertex_array 0;

    (* Reset to default *)
    Gl.active_texture Gl.texture0
end
