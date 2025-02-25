(*
let framebuffer_size_callback _window width height =
  Tgl3.Gl.viewport 0 0 width height;
  ()
*)

let bigarray_create k len = Bigarray.(Array1.create k c_layout len)

let get_int =
  let a = bigarray_create Bigarray.int32 1 in
  fun f -> f a; Int32.to_int a.{0}

let () =
  GLFW.init ();
  GLFW.windowHint ~hint:GLFW.ContextVersionMajor ~value:3;
  GLFW.windowHint ~hint:GLFW.ContextVersionMinor ~value:3;
  GLFW.windowHint ~hint:GLFW.OpenGLProfile ~value:GLFW.CoreProfile;

  let window = GLFW.createWindow ~width:800 ~height:600 ~title:"LearnOpenGL" () in
  GLFW.makeContextCurrent ~window:(Some window);
  (*GLFW.setFramebufferSizeCallback ~window:window ~f:(framebuffer_size_callback);*)

  let vertexShaderSource = {|
    #version 460 core
    layout (location = 0) in vec3 aPos;
    void main()
    {
      gl_Position = vec4(aPos.x, aPos.y, aPos.z, 1.0);
    }
  |} in
  let vertexShader = Tgl3.Gl.create_shader Tgl3.Gl.vertex_shader in
  Tgl3.Gl.shader_source Tgl3.Gl.vertex_shader vertexShaderSource;

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
  in
  Tgl3.Gl.compile_shader Tgl3.Gl.vertex_shader;
  check_shader_compilation vertexShader;

  let fragmentShaderSource = {|
    #version 460 core
    out vec4 FragColor;
    void main()
    {
      FragColor = vec4(1.0, 0.5f, 0.2f, 1.0f);
    }
  |} in
  let fragmentShader = Tgl3.Gl.create_shader Tgl3.Gl.fragment_shader in
  Tgl3.Gl.shader_source Tgl3.Gl.fragment_shader fragmentShaderSource;
  Tgl3.Gl.compile_shader Tgl3.Gl.fragment_shader;
  check_shader_compilation fragmentShader;

  let shaderProgram = Tgl3.Gl.create_program () in
  Tgl3.Gl.attach_shader shaderProgram Tgl3.Gl.vertex_shader;
  Tgl3.Gl.attach_shader shaderProgram Tgl3.Gl.fragment_shader;
  Tgl3.Gl.link_program shaderProgram;

  Tgl3.Gl.delete_shader vertexShader;
  Tgl3.Gl.delete_shader fragmentShader;

  (* 1. Gl bind Vertex Array Object *)
  let vao = get_int (Tgl3.Gl.gen_vertex_arrays 1) in
  Tgl3.Gl.bind_vertex_array vao;

  (* 2. copy our vertices array in a buffer for OpenGL to use*)
  let vbo = get_int (Tgl3.Gl.gen_buffers 1) in
  Tgl3.Gl.bind_buffer Tgl3.Gl.array_buffer vbo;
  let vertices = Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout [|
    -0.5; -0.5; 0.0;
    0.5; -0.5; 0.0;
    0.0;  0.5; 0.0
  |] in
  Tgl3.Gl.buffer_data Tgl3.Gl.array_buffer (Bigarray.Array1.size_in_bytes vertices) (Some vertices) Tgl3.Gl.static_draw;

  (* 3. then set the vertex attributes pointers *)
  Tgl3.Gl.vertex_attrib_pointer 0 3 Tgl3.Gl.float false 3 (`Offset 0);
  Tgl3.Gl.enable_vertex_attrib_array 0;
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