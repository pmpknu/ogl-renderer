open Assimp
open Mesh

module Model = struct
  type t = {
    textures_loaded : MeshTexture.t list;
    meshes : Mesh.t list;
    directory : string;
    gamma_correction : bool;
  }

  let load_texture directory path =
    let texture_id = Texture.load_texture (Filename.concat directory path) in
    MeshTexture.create ~id:texture_id ~type_:"" ~path:(Filename.concat directory path)

  let process_mesh mesh materials _scene_textures textures_loaded =
    let vertices = Array.mapi (fun i _ ->
      let pos = mesh.mesh_vertices.(i) in
      let norm = if i < Array.length mesh.mesh_normals
                 then mesh.mesh_normals.(i) else [|0.;0.;0.|] in
      let tex_coords = if Array.length mesh.mesh_texture_coords > 0
                         && i < Array.length mesh.mesh_texture_coords.(0)
                       then mesh.mesh_texture_coords.(0).(i) else [|0.;0.;0.|] in
      let tangent = try mesh.mesh_tangents.(i) with _ -> [|0.;0.;0.|] in
      let bitangent = try mesh.mesh_bitangents.(i) with _ -> [|0.;0.;0.|] in
      Printf.printf "mesh_vertices length: %d\n" (Array.length mesh.mesh_vertices);
      Printf.printf "mesh_normals length: %d\n" (Array.length mesh.mesh_normals);
      Printf.printf "mesh_texture_coords length: %d\n" (Array.length mesh.mesh_texture_coords);
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
  
    let material = materials.(0) in
    let load_tex tex_type type_name =
      Array.fold_left (fun acc prop ->
        if prop.prop_semantic = tex_type then
          let path = prop.prop_data in
          if List.exists (fun t -> MeshTexture.path t = path) !textures_loaded then
            acc
          else
            let tex = load_texture "" path in
            textures_loaded := tex :: !textures_loaded;
            { tex with MeshTexture.type_ = type_name } :: acc
        else acc
      ) [] material
    in

    let textures = List.concat [
      load_tex texture_type_diffuse "texture_diffuse";
      load_tex texture_type_specular "texture_specular";
      load_tex texture_type_normals "texture_normal";
      load_tex texture_type_height "texture_height";
    ] in
    Mesh.create ~vertices 
    ~indices:(Array.of_list (Array.to_list indices))
    ~textures:(Array.of_list textures)

    let process_node root_node scene textures_loaded =
      let rec process_stack stack acc =
        match stack with
        | [] -> acc
        | node :: rest ->
          let meshes = Array.fold_left (fun acc mesh_idx ->
            let mesh = scene.scene_meshes.(mesh_idx) in
            let materials = scene.scene_materials in
            process_mesh mesh materials scene.scene_textures textures_loaded :: acc
          ) acc node.node_meshes in
          let new_stack = Array.fold_left (fun stack child ->
            child :: stack
          ) rest node.node_children in
          process_stack new_stack meshes
      in
      process_stack [root_node] []

  let aiProcess_Triangulate = 0x8
  let aiProcess_FlipUVs = 0x800000
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