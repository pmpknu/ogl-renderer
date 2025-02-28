open Render
open Math.Mat
open Math.Cam
open Math.Vec

let glsl_version gl_version = match gl_version with
  | 3,2 -> "150" | 3,3 -> "330"
  | 4,0 -> "400" | 4,1 -> "410" | 4,2 -> "420" | 4,3 -> "430" | 4,4 -> "440" | 4,5 -> "450" | 4,6 -> "460"
  | _ -> assert false

(* Vertex and Fragment Shader Sources *)
let vertex_shader v = Printf.sprintf "
    #version %s core
    in vec3 vertex;
    in vec3 color;
    in vec2 texCoord;
    uniform mat4 model;
    uniform mat4 view;
    uniform mat4 projection;
    out vec4 v_color;
    out vec2 TexCoord;
    void main()
    {
      v_color = vec4(color, 1.0);
      gl_Position = projection * view * model * vec4(vertex, 1.0);
      TexCoord = texCoord;
    }" v

let fragment_shader v = Printf.sprintf "
    #version %s core
    in vec4 v_color;
    in vec2 TexCoord;
    uniform sampler2D texture1;
    out vec4 color;
    void main()
    {
      color = texture(texture1, TexCoord) * v_color;
    }" v


let vertices = Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout [|
    (* positions    |      colors       | texture coords *)
    0.5;  0.5; 0.0;   1.0; 0.0; 0.0;   1.0; 1.0; (* top right *)
    0.5; -0.5; 0.0;   0.0; 1.0; 0.0;   1.0; 0.0; (* bottom right *)
   -0.5; -0.5; 0.0;   0.0; 0.0; 1.0;   0.0; 0.0; (* bottom left *)
   -0.5;  0.5; 0.0;   1.0; 1.0; 0.0;   0.0; 1.0; (* top left *)
|]

let indices = Bigarray.Array1.of_array Bigarray.int32 Bigarray.c_layout [|
  Int32.of_int 0; Int32.of_int 1; Int32.of_int 3;
  Int32.of_int 1; Int32.of_int 2; Int32.of_int 3;
|]

let camera_pos = ref (0.0, 0.0, -3.0)
let camera_front = ref (0.0, 0.0, -1.0)
let camera_up = ref (0.0, 1.0, 0.0)
let camera_speed = 0.05
let first_mouse = ref true
let last_x = ref 0.0
let last_y = ref 0.0
let yaw = ref (-90.0)
let pitch = ref 0.0
let radians degrees = degrees *. Float.pi /. 180.0

let mouse_callback _ xpos_in ypos_in =
  let xpos = xpos_in in
  let ypos = ypos_in in

  if !first_mouse then (
    last_x := xpos;
    last_y := ypos;
    first_mouse := false
  );

  let xoffset = xpos -. !last_x in
  let yoffset = !last_y -. ypos in
  last_x := xpos;
  last_y := ypos;

  let sensitivity = 0.1 in
  let xoffset = xoffset *. sensitivity in
  let yoffset = yoffset *. sensitivity in

  yaw := !yaw +. xoffset;
  pitch := !pitch +. yoffset;

  let pitch = if !pitch > 89.0 then 89.0 else if !pitch < -89.0 then -89.0 else !pitch in

  let front_x = cos (radians !yaw) *. cos (radians pitch) in
  let front_y = sin (radians pitch) in
  let front_z = sin (radians !yaw) *. cos (radians pitch) in
  let front = (front_x, front_y, front_z) in
  camera_front := Vec3.normalize front

let process_input window =
  if GLFW.getKey ~window:window ~key:GLFW.Escape then
    GLFW.setWindowShouldClose ~window:window ~b:true;

  let right = Vec3.normalize (Vec3.cross !camera_front !camera_up) in
  if GLFW.getKey ~window:window ~key:GLFW.W then
    camera_pos := Vec3.sub !camera_pos (Vec3.mul !camera_front camera_speed);
  if GLFW.getKey ~window:window ~key:GLFW.S then
    camera_pos := Vec3.add !camera_pos (Vec3.mul !camera_front camera_speed);
  if GLFW.getKey ~window:window ~key:GLFW.D then
    camera_pos := Vec3.add !camera_pos (Vec3.mul right camera_speed);
  if GLFW.getKey ~window:window ~key:GLFW.A then
    camera_pos := Vec3.sub !camera_pos (Vec3.mul right camera_speed);

  if GLFW.getMouseButton ~window:window ~button:GLFW.mouse_button_left then
    ignore (GLFW.setCursorPosCallback ~window ~f:(Some mouse_callback))
  else
    ignore (GLFW.setCursorPosCallback ~window ~f:(Some (fun _ _ _ -> (last_x := 0.0; last_y := 0.0; first_mouse := true))))

let () =
  GLFW.init ();
  GLFW.windowHint ~hint:GLFW.ContextVersionMajor ~value:3;
  GLFW.windowHint ~hint:GLFW.ContextVersionMinor ~value:3;
  GLFW.windowHint ~hint:GLFW.OpenGLProfile ~value:GLFW.CoreProfile;

  let window = GLFW.createWindow ~width:800 ~height:600 ~title:"ocaml opengl" () in
  GLFW.makeContextCurrent ~window:(Some window);

  let vertexShaderSource = vertex_shader (glsl_version (4, 6)) in
  let fragmentShaderSource = fragment_shader (glsl_version (4, 6)) in
  let shaderProgram = Shader.create_shader_program vertexShaderSource fragmentShaderSource in

  (* 1. Bind Vertex Array Object *)
  let vao = Shader.get_int (Tgl3.Gl.gen_vertex_arrays 1) in
  Tgl3.Gl.bind_vertex_array vao;

  (* 2. Copy vertices into a buffer *)
  let vbo = Shader.get_int (Tgl3.Gl.gen_buffers 1) in
  Tgl3.Gl.bind_buffer Tgl3.Gl.array_buffer vbo;
  Tgl3.Gl.buffer_data Tgl3.Gl.array_buffer (Bigarray.Array1.size_in_bytes vertices) (Some vertices) Tgl3.Gl.static_draw;

  (* 3. Copy indices into an element buffer *)
  let ebo = Shader.get_int (Tgl3.Gl.gen_buffers 1) in
  Tgl3.Gl.bind_buffer Tgl3.Gl.element_array_buffer ebo;
  Tgl3.Gl.buffer_data Tgl3.Gl.element_array_buffer (Bigarray.Array1.size_in_bytes indices) (Some indices) Tgl3.Gl.static_draw;

  (* 4. Set vertex attributes *)
  Tgl3.Gl.vertex_attrib_pointer 0 3 Tgl3.Gl.float false (8 * 4) (`Offset 0);
  Tgl3.Gl.enable_vertex_attrib_array 0;

  Tgl3.Gl.vertex_attrib_pointer 1 3 Tgl3.Gl.float false (8 * 4) (`Offset (3 * 4));
  Tgl3.Gl.enable_vertex_attrib_array 1;

  Tgl3.Gl.vertex_attrib_pointer 2 2 Tgl3.Gl.float false (8 * 4) (`Offset (6 * 4));
  Tgl3.Gl.enable_vertex_attrib_array 2;

  Tgl3.Gl.bind_buffer Tgl3.Gl.array_buffer 0;
  (* 5. Unbind VAO (it's always a good thing to unbind any buffer/array to prevent strange bugs) *)
  Tgl3.Gl.bind_vertex_array 0;

  (* 5.5 wireframe mode *)
  (* Tgl3.Gl.polygon_mode Tgl3.Gl.front_and_back Tgl3.Gl.line; *)

  (* Load Texture *)
  let texture = Texture.load_texture "resources/albedo.jpg" in
  Shader.use shaderProgram;
  Tgl3.Gl.uniform1i (Tgl3.Gl.get_uniform_location shaderProgram "texture1") 0;

  while not (GLFW.windowShouldClose ~window:window) do
    process_input window;

    Tgl3.Gl.clear_color 0.239 0.30 0.49 1.0;
    Tgl3.Gl.clear Tgl3.Gl.color_buffer_bit;

    (* Bind Texture *)
    Tgl3.Gl.active_texture Tgl3.Gl.texture0;
    Texture.bind texture;

    (* Use Shader Program *)
    Shader.use shaderProgram;

    (* Set Uniforms *)
    (* Rotate the model matrix *)
    let model = Matr.rotate (1.0, 0.0, 0.0) (-55.0) in

    (* Create the projection matrix *)
    let camera = Camera.create () in
    let camera = Camera.set_loc_at_up camera !camera_pos (Vec3.add !camera_pos !camera_front) !camera_up in

    (* Retrieve the matrix uniform locations *)
    let view = Camera.get_view camera in
    let projection = Camera.get_proj camera in

    (* Retrieve the matrix uniform locations *)
    let model_loc = Tgl3.Gl.get_uniform_location shaderProgram "model" in
    let view_loc = Tgl3.Gl.get_uniform_location shaderProgram "view" in
    let projection_loc = Tgl3.Gl.get_uniform_location shaderProgram "projection" in

    (* Pass the matrices to the shaders *)
    Tgl3.Gl.uniform_matrix4fv model_loc 1 false (Matr.of_bigarray model);
    Tgl3.Gl.uniform_matrix4fv view_loc 1 false (Matr.of_bigarray view);
    Tgl3.Gl.uniform_matrix4fv projection_loc 1 false (Matr.of_bigarray projection);

    (* Draw *)
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
