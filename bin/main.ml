let main () =
  ignore (Glut.init ~argv:Sys.argv);
  Glut.initDisplayMode ~alpha:true ~depth:true ();
  Glut.initWindowSize ~w:500 ~h:500;
  ignore (Glut.createWindow ~title:"lablglut & LablGL");

  let vertexShaderSource = {|
    #version 460 core
    layout (location = 0) in vec3 aPos;
    void main()
    {
      gl_Position = vec4(aPos.x, aPos.y, aPos.z, 1.0);
    }
  |} in
  let vertexShader = GlShader.create ~shader_type:`vertex_shader in
  GlShader.source ~shader:vertexShader vertexShaderSource;
  GlShader.compile ~shader:vertexShader;

  let fragmentShaderSource = {|
    #version 460 core
    out vec4 FragColor;
    void main()
    {
      FragColor = vec4(1.0, 0.5f, 0.2f, 1.0f);
    }
  |} in
  let fragmentShader = GlShader.create  ~shader_type:`fragment_shader in
  GlShader.source ~shader:fragmentShader fragmentShaderSource;
  GlShader.compile ~shader:fragmentShader;

  let shaderProgram = GlShader.create_program () in
  GlShader.attach ~program:shaderProgram ~shader:vertexShader;
  GlShader.attach ~program:shaderProgram ~shader:fragmentShader;
  GlShader.link_program ~program:shaderProgram;

  GlShader.delete ~shader:vertexShader;
  GlShader.delete ~shader:fragmentShader;

  (* 1. Gl bind Vertex Array Object *)
  let vao = GlArray.gen_vertex_array () in
  GlArray.bind_vertex_array vao;

  (* 2. copy our vertices array in a buffer for OpenGL to use*)
  let vbo = GlArray.create_buffer () in
  GlArray.bind_buffer `array vbo;
  let vertices = Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout [|
    -0.5; -0.5; 0.0;
    0.5; -0.5; 0.0;
    0.0;  0.5; 0.0
  |] in
  GlArray.buffer `array (Bigarray.Array1.dim vertices * 4) (`data (Bigarray.genarray_of_array1 vertices)) `static_draw;


  (* 3. then set the vertex attributes pointers *)
  GlArray.vertex_attrib_pointer 0 3 `float false 0 (`offset 0);
  GlArray.enable_vertex_attrib_array 0;

  Glut.displayFunc ~cb:(fun () ->
      (* display callback *)
      GlClear.color (0.239, 0.30, 0.64);
      GlClear.clear [ `color ];
      GlDraw.color (1.0, 1.0, 1.0);
      GlMat.mode `projection;

      (* 4. use our shader program when we want to render an object *)
      GlShader.use_program shaderProgram;
      GlArray.bind_vertex_array vao;

      (* 5. now draw the object *)
      GlDraw.draw_arrays `triangles 0 3;

      Gl.flush ();
      Glut.swapBuffers ()
      );
  (* ignore (Timer.add ~ms:10000 ~callback:(fun () -> exit 0));  *)
  Glut.mainLoop ()

let _ = main ()
