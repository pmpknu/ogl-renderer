(* Helper function to create Bigarray *)
let bigarray_create kind len = Bigarray.(Array1.create kind c_layout len)

(* Helper function to get integer values from OpenGL *)
let get_int f =
  let a = bigarray_create Bigarray.int32 1 in
  f a;
  Int32.to_int a.{0}

(* Function to check shader compilation status *)
let check_shader_compilation shader =
  let status = bigarray_create Bigarray.int32 1 in
  Tgl3.Gl.get_shaderiv shader Tgl3.Gl.compile_status status;
  if Int32.to_int status.{0} = 0 then (
    let log_length = bigarray_create Bigarray.int32 1 in
    Tgl3.Gl.get_shaderiv shader Tgl3.Gl.info_log_length log_length;
    let length = Int32.to_int log_length.{0} in
    let log = bigarray_create Bigarray.char length in
    Tgl3.Gl.get_shader_info_log shader length None log;
    let log_str = Bytes.create length in
    for i = 0 to length - 1 do
      Bytes.set log_str i (Bigarray.Array1.get log i)
    done;
    failwith ("Shader compilation failed: " ^ Bytes.to_string log_str))

(* Function to check program linking status *)
let check_program_linking program =
  let status = bigarray_create Bigarray.int32 1 in
  Tgl3.Gl.get_programiv program Tgl3.Gl.link_status status;
  if Int32.to_int status.{0} = 0 then (
    let log_length = bigarray_create Bigarray.int32 1 in
    Tgl3.Gl.get_programiv program Tgl3.Gl.info_log_length log_length;
    let length = Int32.to_int log_length.{0} in
    let log = bigarray_create Bigarray.char length in
    Tgl3.Gl.get_program_info_log program length None log;
    let log_str = Bytes.create length in
    for i = 0 to length - 1 do
      Bytes.set log_str i (Bigarray.Array1.get log i)
    done;
    failwith ("Program linking failed: " ^ Bytes.to_string log_str))

(* Function to create and compile a shader *)
let create_shader shader_type source =
  let shader = Tgl3.Gl.create_shader shader_type in
  Tgl3.Gl.shader_source shader source;
  Tgl3.Gl.compile_shader shader;
  check_shader_compilation shader;
  shader

(* Function to create and link a shader program *)
let create_shader_program vertex_src fragment_src =
  let vertex_shader = create_shader Tgl3.Gl.vertex_shader vertex_src in
  let fragment_shader = create_shader Tgl3.Gl.fragment_shader fragment_src in

  let shader_program = Tgl3.Gl.create_program () in
  Tgl3.Gl.attach_shader shader_program vertex_shader;
  Tgl3.Gl.attach_shader shader_program fragment_shader;
  Tgl3.Gl.link_program shader_program;
  check_program_linking shader_program;

  Tgl3.Gl.delete_shader vertex_shader;
  Tgl3.Gl.delete_shader fragment_shader;
  shader_program

(* Function to use the shader program *)
let use program = Tgl3.Gl.use_program program

(* Utility functions for setting uniform variables *)
let set_uniform_bool program name value =
  let location = Tgl3.Gl.get_uniform_location program name in
  Tgl3.Gl.uniform1i location (if value then 1 else 0)

let set_uniform_int program name value =
  let location = Tgl3.Gl.get_uniform_location program name in
  Tgl3.Gl.uniform1i location value

let set_uniform_float program name value =
  let location = Tgl3.Gl.get_uniform_location program name in
  Tgl3.Gl.uniform1f location value
