open Assimp
open Mesh
open Texture

module Model = struct
  type t = {
    textures_loaded : Texture.t list;
    meshes : Mesh.t list;
    directory : string;
    gamma_correction : bool;
  }

  let load_texture directory path =
    (* Simplified: Assume Texture.create handles loading from path *)
    Texture.create ~id:0 ~type_:"" ~path:(Filename.concat directory path)

  let rec process_node node scene textures_loaded =
    let meshes = Array.fold_left (fun acc mesh_idx ->
      let mesh = scene.scene_meshes.(mesh_idx) in
      let materials = scene.scene_materials in
      process_mesh mesh materials scene.scene_textures textures_loaded :: acc
    ) [] node.node_meshes in
    Array.fold_left (fun acc child ->
      process_node child scene textures_loaded @ acc
    ) meshes node.node_children

  and process_mesh mesh materials scene_textures textures_loaded =
    let vertices = Array.mapi (fun i _ ->
      let pos = mesh.mesh_vertices.(i) in
      let norm = try mesh.mesh_normals.(i) with _ -> [|0.;0.;0.|] in
      let tex_coords = try mesh.mesh_texture_coords.(0).(i) with _ -> [|0.;0.|] in
      let tangent = try mesh.mesh_tangents.(i) with _ -> [|0.;0.;0.|] in
      let bitangent = try mesh.mesh_bitangents.(i) with _ -> [|0.;0.;0.|] in
      Vertex.create
        ~position:(pos.(0), pos.(1), pos.(2))
        ~normal:(norm.(0), norm.(1), norm.(2))
        ~tex_coords:(tex_coords.(0), tex_coords.(1))
        ~tangent:(tangent.(0), tangent.(1), tangent.(2))
        ~bitangent:(bitangent.(0), bitangent.(1), bitangent.(2))
        ~bone_ids:[|0;0;0;0|]
        ~weights:[|0.;0.;0.;0.|]
    ) mesh.mesh_vertices in

    let indices = Array.concat (Array.to_list (Array.map (fun f -> f) mesh.mesh_faces)) in

    let material = materials.(mesh.material_index) in
    let load_tex tex_type type_name =
      Array.fold_left (fun acc prop ->
        if prop.prop_semantic = tex_type then
          let path = prop.prop_data in
          if List.exists (fun t -> t.Texture.path = path) !textures_loaded then
            acc
          else
            let tex = load_texture "" path in
            textures_loaded := tex :: !textures_loaded;
            { tex with type_ = type_name } :: acc
        else acc
      ) [] material
    in

    let textures = List.concat [
      load_tex texture_type_diffuse "texture_diffuse";
      load_tex texture_type_specular "texture_specular";
      load_tex texture_type_normals "texture_normal";
      load_tex texture_type_height "texture_height";
    ] in

    Mesh.create ~vertices:(Array.to_list vertices) 
      ~indices:(Array.to_list indices) 
      ~textures

  let load_model path gamma =
    match import_file path (aiProcess_Triangulate lor aiProcess_FlipUVs) with
    | Error (`Msg e) -> failwith ("Assimp error: " ^ e)
    | Ok raw_scene ->
      let scene = view_scene raw_scene in
      let textures_loaded = ref [] in
      let meshes = process_node scene.scene_root scene textures_loaded in
      release_scene raw_scene;
      { textures_loaded = !textures_loaded; meshes; 
        directory = Filename.dirname path; gamma_correction = gamma }

  let create path gamma = load_model path gamma

  let draw model shader =
    List.iter (fun mesh -> Mesh.draw mesh shader) model.meshes
end