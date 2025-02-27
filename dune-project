(lang dune 3.17)

(name ogl_renderer)

(generate_opam_files true)

(source
 (github pmpknu/ogl-renderer))

(authors "Dan Gorlyakov <pmpknu@gmail.com>")

(maintainers "Dan Gorlyakov <pmpknu@gmail.com>")

(license MIT)

(package
 (name ogl_renderer)
 (synopsis "OCaml OpenGL renderer")
 (description "This is a simple OpenGL renderer written in OCaml.")
 (depends
  ocaml
  ppx_inline_test
 )
 (tags
  ("opengl" "renderer" )))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
