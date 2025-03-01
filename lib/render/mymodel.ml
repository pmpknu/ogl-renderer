open Printf
open Math.Vec
open Mesh

(* Basic vector types *)
type vec2 = Vec2.t
type vec3 = Vec3.t

(* Basic mesh structures *)
type vertex = {
  position : vec3;
  normal : vec3;
  tex_coords : vec2;
}

type mesh = {
  vertices : vertex array;
  indices : int array;
}

type model = {
  meshes : Mesh.t array;
  path : string;
}

(* Shader program type - this would be your GL shader program ID *)
type shader_program = int

(* Helper functions *)
let split_on_char sep s =
  let rec aux i acc =
    try
      let j = String.index_from s i sep in
      aux (j + 1) (String.sub s i (j - i) :: acc)
    with Not_found ->
      List.rev (String.sub s i (String.length s - i) :: acc)
  in
  aux 0 []

let parse_float s =
  try float_of_string s
  with _ -> 0.0

(* OBJ Parser *)
module ObjParser = struct
  let parse_line line positions normals texcoords faces =
    let parts = split_on_char ' ' line in
    match parts with
    | "v" :: x :: y :: z :: _ ->
        (parse_float x, parse_float y, parse_float z) :: positions, normals, texcoords, faces
    | "vn" :: x :: y :: z :: _ ->
        positions, (parse_float x, parse_float y, parse_float z) :: normals, texcoords, faces
    | "vt" :: u :: v :: _ ->
        positions, normals, (parse_float u, parse_float v) :: texcoords, faces
    | "f" :: vertices ->
        let parse_vertex_indices vertex =
          let indices = split_on_char '/' vertex in
          match indices with
          | [pos_idx; tex_idx; norm_idx] ->
              (int_of_string pos_idx - 1, 
               int_of_string tex_idx - 1, 
               int_of_string norm_idx - 1)
          | [pos_idx; tex_idx] ->
              (int_of_string pos_idx - 1, 
               int_of_string tex_idx - 1, 
               0) (* Default normal index *)
          | [pos_idx] ->
              (int_of_string pos_idx - 1, 
               0, (* Default texture index *)
               0) (* Default normal index *)
          | _ -> (0, 0, 0) (* Default indices *)
        in
        let vertex_indices = List.map parse_vertex_indices vertices in
        positions, normals, texcoords, vertex_indices :: faces
    | _ -> positions, normals, texcoords, faces

  let parse_obj filename =
    try
      let ic = open_in filename in
      let rec read_lines positions normals texcoords faces =
        try
          let line = input_line ic in
          let line = String.trim line in
          if line = "" || line.[0] = '#' then
            read_lines positions normals texcoords faces
          else
            let positions', normals', texcoords', faces' = 
              parse_line line positions normals texcoords faces 
            in
            read_lines positions' normals' texcoords' faces'
        with End_of_file ->
          close_in ic;
          List.rev positions, List.rev normals, List.rev texcoords, List.rev faces
      in
      let positions, normals, texcoords, faces = read_lines [] [] [] [] in
      
      (* Convert faces to vertices and indices *)
      let vertices = Array.make (List.length faces * 3) {
        position = (0.0, 0.0, 0.0);
        normal = (0.0, 0.0, 0.0);
        tex_coords = (0.0, 0.0);
      } in
      
      let indices = Array.init (List.length faces * 3) (fun i -> i) in
      
      let positions_array = Array.of_list positions in
      let normals_array = Array.of_list normals in
      let texcoords_array = Array.of_list texcoords in
      
      List.iteri (fun face_idx face_vertices ->
        List.iteri (fun vertex_idx (pos_idx, tex_idx, norm_idx) ->
          let idx = face_idx * 3 + vertex_idx in
          let position = 
            if pos_idx >= 0 && pos_idx < Array.length positions_array then
              positions_array.(pos_idx)
            else (0.0, 0.0, 0.0)
          in
          let normal =
            if norm_idx >= 0 && norm_idx < Array.length normals_array then
              normals_array.(norm_idx)
            else (0.0, 1.0, 0.0) (* Default up vector *)
          in
          let tex_coords =
            if tex_idx >= 0 && tex_idx < Array.length texcoords_array then
              texcoords_array.(tex_idx)
            else (0.0, 0.0)
          in
          vertices.(idx) <- {
            position;
            normal;
            tex_coords;
          }
        ) (List.fold_left (fun acc x -> if List.length acc < 3 then x :: acc else acc) [] face_vertices |> List.rev);
      ) faces;
      
      (* Convert vertices to Mesh.Vertex.t *)
      let mesh_vertices = Array.map (fun v ->
        Vertex.create 
          ~position:(v.position)
          ~normal:(v.normal)
          ~tex_coords:(v.tex_coords)
          ~tangent:Vec3.zero
          ~bitangent:Vec3.zero
          ~bone_ids:[|0; 0; 0; 0|]
          ~weights:[|0.0; 0.0; 0.0; 0.0|]
      ) vertices in
      
      (* Create a mesh using the Mesh module *)
      Mesh.create ~vertices:mesh_vertices ~indices ~textures:[||]
    with
    | Sys_error msg -> 
        printf "Error opening file: %s\n" msg;
        Mesh.create ~vertices:[||] ~indices:[||] ~textures:[||]
    | e -> 
        printf "Error parsing OBJ file: %s\n" (Printexc.to_string e);
        Mesh.create ~vertices:[||] ~indices:[||] ~textures:[||]
end

(* Model module *)
module Model = struct
  let create filename =
    let mesh = ObjParser.parse_obj filename in
    let model = {
      meshes = [| mesh |];
      path = filename;
    } in
    model
  
  let draw model shader_program =
    (* For each mesh in the model *)
    Array.iter (fun mesh ->
      Mesh.draw mesh shader_program;
    ) model.meshes
end