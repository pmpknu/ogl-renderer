open Render

let glsl_version gl_version = match gl_version with
  | 3,2 -> "150" | 3,3 -> "330"
  | 4,0 -> "400" | 4,1 -> "410" | 4,2 -> "420" | 4,3 -> "430" | 4,4 -> "440" | 4,5 -> "450" | 4,6 -> "460"
  | _ -> assert false

(* Vertex and Fragment Shader Sources *)
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

let vertices = Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout [|
    (* positions    |      colors *)
    0.5;  0.5; 0.0;   1.0; 0.0; 0.0; (* top right *)
    0.5; -0.5; 0.0;   0.0; 1.0; 0.0; (* bottom right *)
   -0.5; -0.5; 0.0;   0.0; 0.0; 1.0; (* bottom left *)
   -0.5;  0.5; 0.0;   1.0; 1.0; 0.0; (* top left *)
|]

let indices = Bigarray.Array1.of_array Bigarray.int32 Bigarray.c_layout [|
  Int32.of_int 0; Int32.of_int 1; Int32.of_int 3;
  Int32.of_int 1; Int32.of_int 2; Int32.of_int 3;
|]

let () =
  GLFW.init ();
  GLFW.windowHint ~hint:GLFW.ContextVersionMajor ~value:3;
  GLFW.windowHint ~hint:GLFW.ContextVersionMinor ~value:3;
  GLFW.windowHint ~hint:GLFW.OpenGLProfile ~value:GLFW.CoreProfile;

  let window = GLFW.createWindow ~width:800 ~height:600 ~title:"LearnOpenGL" () in
  GLFW.makeContextCurrent ~window:(Some window);
  (*GLFW.setFramebufferSizeCallback ~window:window ~f:(framebuffer_size_callback);*)

  let vertexShaderSource = vertex_shader (glsl_version (4, 6)) in
  let fragmentShaderSource = fragment_shader (glsl_version (4, 6)) in
  let shaderProgram = Shader.create_shader_program vertexShaderSource fragmentShaderSource in

  (* 1. Gl bind Vertex Array Object *)
  let vao = Shader.get_int (Tgl3.Gl.gen_vertex_arrays 1) in
  Tgl3.Gl.bind_vertex_array vao;

  (* 2. copy our vertices array in a buffer for OpenGL to use*)
  let vbo = Shader.get_int (Tgl3.Gl.gen_buffers 1) in
  Tgl3.Gl.bind_buffer Tgl3.Gl.array_buffer vbo;
  Tgl3.Gl.buffer_data Tgl3.Gl.array_buffer (Bigarray.Array1.size_in_bytes vertices) (Some vertices) Tgl3.Gl.static_draw;

  (* 3. copy our index array in a element buffer for OpenGL to use*)
  let ebo = Shader.get_int (Tgl3.Gl.gen_buffers 1) in
  Tgl3.Gl.bind_buffer Tgl3.Gl.element_array_buffer ebo;
  Tgl3.Gl.buffer_data Tgl3.Gl.element_array_buffer (Bigarray.Array1.size_in_bytes indices) (Some indices) Tgl3.Gl.static_draw;

  (* 4. then set the vertex attributes pointers *)
  Tgl3.Gl.vertex_attrib_pointer 0 3 Tgl3.Gl.float false (6 * 4) (`Offset 0);
  Tgl3.Gl.enable_vertex_attrib_array 0;

  Tgl3.Gl.vertex_attrib_pointer 1 3 Tgl3.Gl.float false (6 * 4) (`Offset (3 * 4));
  Tgl3.Gl.enable_vertex_attrib_array 1;

  Tgl3.Gl.bind_buffer Tgl3.Gl.array_buffer 0;
  (* 5. Unbind VAO (it's always a good thing to unbind any buffer/array to prevent strange bugs) *)
  Tgl3.Gl.bind_vertex_array 0;

  (* 5.5 wireframe mode *)
  (* Tgl3.Gl.polygon_mode Tgl3.Gl.front_and_back Tgl3.Gl.line; *)

  while not (GLFW.windowShouldClose ~window:window) do
    Tgl3.Gl.clear_color 0.239 0.30 0.49 1.0;
    Tgl3.Gl.clear Tgl3.Gl.color_buffer_bit;

    (* 6. Use our shader program when we want to render an object *)
    Shader.use shaderProgram;
    (* 7. draw the triangle *)
    Tgl3.Gl.bind_vertex_array vao;
    Tgl3.Gl.draw_elements Tgl3.Gl.triangles 6 Tgl3.Gl.unsigned_int (`Offset 0);
    (*Tgl3.Gl.bind_vertex_array 0;*)

    GLFW.swapBuffers ~window:window;
    GLFW.pollEvents ();
  done;

  (* Cleanup *)
  Tgl3.Gl.delete_vertex_arrays 1 (Shader.bigarray_create Bigarray.int32 1);
  Tgl3.Gl.delete_buffers 1 (Shader.bigarray_create Bigarray.int32 1);
  Tgl3.Gl.delete_buffers 1 (Shader.bigarray_create Bigarray.int32 1);
  Tgl3.Gl.delete_program shaderProgram;
  GLFW.terminate ();