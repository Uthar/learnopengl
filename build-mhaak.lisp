(require :asdf)

#+win32
(progn
  (ql:quickload :cl-liballegro)
  (ql:quickload :cl-opengl)
  (ql:quickload :3d-vectors)
  (ql:quickload :3d-matrices))

#-win32
(progn
  (asdf:load-system :cl-liballegro)
  (asdf:load-system :cl-opengl)
  (asdf:load-system :3d-vectors)
  (asdf:load-system :3d-matrices))

(asdf:load-system :mhaak)

(cffi:close-foreign-library 'al::liballegro_acodec)
(cffi:close-foreign-library 'al::liballegro_audio)
(cffi:close-foreign-library 'al::liballegro_color)
(cffi:close-foreign-library 'al::liballegro_dialog)
(cffi:close-foreign-library 'al::liballegro_font)
(cffi:close-foreign-library 'al::liballegro_image)
(cffi:close-foreign-library 'al::liballegro_memfile)
(cffi:close-foreign-library 'al::liballegro_physfs)
(cffi:close-foreign-library 'al::liballegro_primitives)
(cffi:close-foreign-library 'al::liballegro_ttf)
(cffi:close-foreign-library 'al::liballegro_video)
(cffi:close-foreign-library 'cffi::libffi)

(sb-ext:save-lisp-and-die
 #-win32 "mhaak"
 #+win32 "mhaak.exe"

 :toplevel 'learnopengl::run-example
 :executable t

 #+sb-core-compression :compression
 #+sb-core-compression 9

 #+win32 :application-type
 #+win32 :gui)
