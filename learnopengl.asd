

(defsystem :learnopengl
  :depends-on (
               :alexandria
               :cl-liballegro
               :cl-opengl
               :3d-matrices
               :3d-vectors
               :cl-jpeg
               :png-read
               )
  :components ((:file "common")
               (:file "shader")
               (:file "3d")
               (:file "texture")))
