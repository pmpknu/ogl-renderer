(*
let framebuffer_size_callback _window width height =
  Tgl3.Gl.viewport 0 0 width height;
  ()
*)

let bigarray_create k len = Bigarray.(Array1.create k c_layout len)

let get_int =
  let a = bigarray_create Bigarray.int32 1 in
  fun f -> f a; Int32.to_int a.{0}

let glsl_version gl_version = match gl_version with
  | 3,2 -> "150" | 3,3 -> "330"
  | 4,0 -> "400" | 4,1 -> "410" | 4,2 -> "420" | 4,3 -> "430" | 4,4 -> "440" | 4,5 -> "450" | 4,6 -> "460"
  | _ -> assert false

let check_shader_compilation shader =
  let open Tgl3.Gl in
  let status = bigarray_create Bigarray.int32 1 in
  get_shaderiv shader compile_status status;
  if Int32.to_int status.{0} = 0 then (
    let log_length = bigarray_create Bigarray.int32 1 in
    get_shaderiv shader info_log_length log_length;
    let length = Int32.to_int log_length.{0} in
    let log = bigarray_create Bigarray.char length in
    get_shader_info_log shader length None log;
    let log_str = Bytes.create length in
    for i = 0 to length - 1 do
      Bytes.set log_str i (Bigarray.Array1.get log i)
    done;
    Printf.eprintf "Shader compilation failed: %s\n" (Bytes.to_string log_str);
  )

let check_program_linking program =
  let open Tgl3.Gl in
  let status = bigarray_create Bigarray.int32 1 in
  get_programiv program link_status status;
  if Int32.to_int status.{0} = 0 then (
    let log_length = bigarray_create Bigarray.int32 1 in
    get_programiv program info_log_length log_length;
    let length = Int32.to_int log_length.{0} in
    let log = bigarray_create Bigarray.char length in
    get_program_info_log program length None log;
    let log_str = Bytes.create length in
    for i = 0 to length - 1 do
      Bytes.set log_str i (Bigarray.Array1.get log i)
    done;
    Printf.eprintf "Program linking failed: %s\n" (Bytes.to_string log_str);
  )

let vertex_shader v = Printf.sprintf "
    #version %s core
    in vec3 vertex;
    in vec3 color;
    out vec4 v_color;
    void main()
    {
      v_color = vec4(color, 1.0);
      gl_Position = vec4(vertex, 1.0);
    }" v
  
let fragment_shader v = Printf.sprintf "
    #version %s core
    in vec4 v_color;
    out vec4 color;
    void main() { color = v_color; }" v

let () =
  GLFW.init ();
  GLFW.windowHint ~hint:GLFW.ContextVersionMajor ~value:3;
  GLFW.windowHint ~hint:GLFW.ContextVersionMinor ~value:3;
  GLFW.windowHint ~hint:GLFW.OpenGLProfile ~value:GLFW.CoreProfile;

  let window = GLFW.createWindow ~width:800 ~height:600 ~title:"LearnOpenGL" () in
  GLFW.makeContextCurrent ~window:(Some window);
  (*GLFW.setFramebufferSizeCallback ~window:window ~f:(framebuffer_size_callback);*)

  let vertexShaderSource = vertex_shader (glsl_version (4, 6)) in
  let vertexShader = Tgl3.Gl.create_shader Tgl3.Gl.vertex_shader in
  Tgl3.Gl.shader_source vertexShader vertexShaderSource;
  Tgl3.Gl.compile_shader vertexShader;
  check_shader_compilation vertexShader;

  let fragmentShaderSource = fragment_shader (glsl_version (4, 6)) in
  let fragmentShader = Tgl3.Gl.create_shader Tgl3.Gl.fragment_shader in
  Tgl3.Gl.shader_source fragmentShader fragmentShaderSource;
  Tgl3.Gl.compile_shader fragmentShader;
  check_shader_compilation fragmentShader;

  let shaderProgram = Tgl3.Gl.create_program () in
  Tgl3.Gl.attach_shader shaderProgram vertexShader;
  Tgl3.Gl.attach_shader shaderProgram fragmentShader;
  Tgl3.Gl.bind_attrib_location shaderProgram 0 "vertex";
  Tgl3.Gl.bind_attrib_location shaderProgram 1 "color";
  Tgl3.Gl.link_program shaderProgram;
  check_program_linking shaderProgram;

  Tgl3.Gl.delete_shader vertexShader;
  Tgl3.Gl.delete_shader fragmentShader;

  (* 1. Gl bind Vertex Array Object *)
  let vao = get_int (Tgl3.Gl.gen_vertex_arrays 1) in
  Tgl3.Gl.bind_vertex_array vao;

  (* 2. copy our vertices array in a buffer for OpenGL to use*)
  let vbo = get_int (Tgl3.Gl.gen_buffers 1) in
  Tgl3.Gl.bind_buffer Tgl3.Gl.array_buffer vbo;
  let vertices = Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout [|
    -0.5; -0.5; 0.0;        1.0; 0.0; 0.0;
     0.5; -0.5; 0.0;        0.0; 1.0; 0.0;
     0.0;  0.5; 0.0;        0.0; 0.0; 1.0;
  |] in
  Tgl3.Gl.buffer_data Tgl3.Gl.array_buffer (Bigarray.Array1.size_in_bytes vertices) (Some vertices) Tgl3.Gl.static_draw;

  (* 3. then set the vertex attributes pointers *)
  Tgl3.Gl.vertex_attrib_pointer 0 3 Tgl3.Gl.float false (6*4) (`Offset 0);
  Tgl3.Gl.enable_vertex_attrib_array 0;

  Tgl3.Gl.vertex_attrib_pointer 1 3 Tgl3.Gl.float false (6 * 4) (`Offset (3 * 4));
  Tgl3.Gl.enable_vertex_attrib_array 1;

  Tgl3.Gl.bind_buffer Tgl3.Gl.array_buffer 0;
  (* 4. Unbind VAO (it's always a good thing to unbind any buffer/array to prevent strange bugs) *)
  Tgl3.Gl.bind_vertex_array 0;

  while not (GLFW.windowShouldClose ~window:window) do
    Tgl3.Gl.clear_color 0.239 0.30 0.49 1.0;
    Tgl3.Gl.clear Tgl3.Gl.color_buffer_bit;

    (* 5. Use our shader program when we want to render an object *)
    Tgl3.Gl.use_program shaderProgram;
    (* 6. draw the triangle *)
    Tgl3.Gl.bind_vertex_array vao;
    Tgl3.Gl.draw_arrays Tgl3.Gl.triangles 0 3;
    (*Tgl3.Gl.bind_vertex_array 0;*)

    GLFW.swapBuffers ~window:window;
    GLFW.pollEvents ();  
  done;

  Tgl3.Gl.delete_vertex_arrays 1 (bigarray_create Bigarray.int32 1);
  Tgl3.Gl.delete_buffers 1 (bigarray_create Bigarray.int32 1);
  Tgl3.Gl.delete_program shaderProgram;
  GLFW.terminate ();