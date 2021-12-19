

(defsystem :learnopengl
  :depends-on (
               :alexandria
               :cl-liballegro
               :cl-opengl
               :3d-matrices
               :3d-vectors
               )
  :components ((:file "common")))
