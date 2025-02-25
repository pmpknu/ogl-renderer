let main () =
  ignore (Glut.init ~argv:Sys.argv);
  Glut.initDisplayMode ~alpha:true ~depth:true ();
  Glut.initWindowSize ~w:500 ~h:500;
  ignore (Glut.createWindow ~title:"lablglut & LablGL");

  let vertexShaderSource = "
    #version 460 core\n
    layout (location = 0) in vec3 aPos;\n
    void main()\n
    {\n
      gl_Position = vec4(aPos.x, aPos.y, aPos.z, 1.0);\n
    }\0";
  let vertexShader = GlShader.create `vertex in
  GlShader.source vertexShader vertexShaderSource;
  GlShader.compile vertexShader;

  let fragmentShaderSource = "
    #version 460 core\n
    out vec4 FragColor;\n
    void main()\n
    {\n
      FragColor = vec4(1.0f, 0.5f, 0.2f, 1.0f);\n
    }\0";
  let fragmentShader = GlShader.create `fragment in
  GlShader.source fragmentShader fragmentShaderSource;
  GlShader.compile fragmentShader;

  let shaderProgram = Gl.create_program () in
  Gl.attach_shader shaderProgram vertexShader;
  Gl.attach_shader shaderProgram fragmentShader;
  Gl.link_program shaderProgram;

  Gl.delete_shader vertexShader;
  Gl.delete_shader fragmentShader;

  let vertices = [|
    -0.5; -0.5; 0.0;
    0.5; -0.5; 0.0;
    0.0;  0.5; 0.0
  |];

  (* 1. Gl bind Vertex Array Object *)
  let vao = GlArray.create_vertex_array () in
  GlArray.bind_vertex_array vao;

  (** 2. copy our vertices array in a buffer for OpenGL to use*)
  let vbo = GlArray.create_buffer () in
  GlArray.bind_buffer `array vbo;
  GlArray.buffer_data `array (`float (`data vertices)) `static_draw;

  (** 3. then set the vertex attributes pointers *)
  GlArray.vertex_attrib_pointer 0 3 `float false 0 (`offset 0);
  GlArray.enable_vertex_attrib_array 0;

  Glut.displayFunc ~cb:(fun () ->
      (* display callback *)
      GlClear.color (0.239, 0.30, 0.64);
      GlClear.clear [ `color ];
      GlDraw.color (1.0, 1.0, 1.0);
      GlMat.mode `projection;

      (** 4. use our shader program when we want to render an object *)
      Gl.use_program shaderProgram;
      GlArray.bind_vertex_array vao;

      (** 5. now draw the object *)
      GlDraw.draw_arrays `triangles 0 3;

      Gl.flush ();
      Glut.swapBuffers ()
      );
  (* ignore (Timer.add ~ms:10000 ~callback:(fun () -> exit 0));  *)
  Glut.mainLoop ()

let _ = main ()
