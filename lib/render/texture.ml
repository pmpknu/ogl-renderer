open Tgl3
open Stb_image

(* Load an image using stb_image *)
let load_image filename =
  match load filename with
  | Error (`Msg msg) -> failwith ("Failed to load texture: " ^ msg)
  | Ok img -> (img.width, img.height, img.channels, img.data)

(* Create a texture from image data *)
let create_texture width height channels data =
  let tex_id = Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout 1 in
  Gl.gen_textures 1 tex_id;
  let texture = Int32.to_int tex_id.{0} in

  Gl.bind_texture Gl.texture_2d texture;

  let internal_format, format =
    match channels with
    | 3 -> (Gl.rgb8, Gl.rgb)
    | 4 -> (Gl.rgba8, Gl.rgba)
    | _ -> failwith "Unsupported image format"
  in

  Gl.tex_image2d Gl.texture_2d 0 internal_format width height 0 format
    Gl.unsigned_byte (`Data data);
  Gl.tex_sub_image2d Gl.texture_2d 0 0 0 width height format Gl.unsigned_byte
    (`Data data);
  Gl.generate_mipmap Gl.texture_2d;

  (* Set texture parameters *)
  Gl.tex_parameteri Gl.texture_2d Gl.texture_mag_filter Gl.linear;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_min_filter Gl.linear_mipmap_linear;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_wrap_s Gl.repeat;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_wrap_t Gl.repeat;

  Gl.bind_texture Gl.texture_2d 0;
  texture

(* Load a texture from a file *)
let load_texture filename =
  let w, h, c, data = load_image filename in
  create_texture w h c data

(* Bind the texture for use *)
let bind texture = Gl.bind_texture Gl.texture_2d texture
