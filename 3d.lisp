(in-package :learnopengl)

(defun cube ()
  (let* ((vao (gl:gen-vertex-array))
         (vbo (gl:gen-buffer))
         (vertices
                   ;; positions    ;; normals     ;; texture coords
           (vector -0.5 -0.5 -0.5  0.0  0.0 -1.0  0.0 0.0
                    0.5 -0.5 -0.5  0.0  0.0 -1.0  1.0 0.0
                    0.5  0.5 -0.5  0.0  0.0 -1.0  1.0 1.0
                    0.5  0.5 -0.5  0.0  0.0 -1.0  1.0 1.0
                   -0.5  0.5 -0.5  0.0  0.0 -1.0  0.0 1.0
                   -0.5 -0.5 -0.5  0.0  0.0 -1.0  0.0 0.0

                   -0.5 -0.5  0.5  0.0  0.0  1.0  0.0 0.0
                    0.5 -0.5  0.5  0.0  0.0  1.0  1.0 0.0
                    0.5  0.5  0.5  0.0  0.0  1.0  1.0 1.0
                    0.5  0.5  0.5  0.0  0.0  1.0  1.0 1.0
                   -0.5  0.5  0.5  0.0  0.0  1.0  0.0 1.0
                   -0.5 -0.5  0.5  0.0  0.0  1.0  0.0 0.0

                   -0.5  0.5  0.5 -1.0  0.0  0.0  1.0 0.0
                   -0.5  0.5 -0.5 -1.0  0.0  0.0  1.0 1.0
                   -0.5 -0.5 -0.5 -1.0  0.0  0.0  0.0 1.0
                   -0.5 -0.5 -0.5 -1.0  0.0  0.0  0.0 1.0
                   -0.5 -0.5  0.5 -1.0  0.0  0.0  0.0 0.0
                   -0.5  0.5  0.5 -1.0  0.0  0.0  1.0 0.0

                    0.5  0.5  0.5  1.0  0.0  0.0  1.0 0.0
                    0.5  0.5 -0.5  1.0  0.0  0.0  1.0 1.0
                    0.5 -0.5 -0.5  1.0  0.0  0.0  0.0 1.0
                    0.5 -0.5 -0.5  1.0  0.0  0.0  0.0 1.0
                    0.5 -0.5  0.5  1.0  0.0  0.0  0.0 0.0
                    0.5  0.5  0.5  1.0  0.0  0.0  1.0 0.0

                   -0.5 -0.5 -0.5  0.0 -1.0  0.0  0.0 1.0
                    0.5 -0.5 -0.5  0.0 -1.0  0.0  1.0 1.0
                    0.5 -0.5  0.5  0.0 -1.0  0.0  1.0 0.0
                    0.5 -0.5  0.5  0.0 -1.0  0.0  1.0 0.0
                   -0.5 -0.5  0.5  0.0 -1.0  0.0  0.0 0.0
                   -0.5 -0.5 -0.5  0.0 -1.0  0.0  0.0 1.0

                   -0.5  0.5 -0.5  0.0  1.0  0.0  0.0 1.0
                    0.5  0.5 -0.5  0.0  1.0  0.0  1.0 1.0
                    0.5  0.5  0.5  0.0  1.0  0.0  1.0 0.0
                    0.5  0.5  0.5  0.0  1.0  0.0  1.0 0.0
                   -0.5  0.5  0.5  0.0  1.0  0.0  0.0 0.0
                   -0.5  0.5 -0.5  0.0  1.0  0.0  0.0 1.0)))

    (gl:bind-vertex-array vao)

    (gl:bind-buffer :array-buffer vbo)
    (gl:buffer-data :array-buffer :static-draw (gl-array :float vertices))

    (gl:vertex-attrib-pointer 0 3 :float nil
                              (* (cffi:foreign-type-size :float) 8)
                              (* (cffi:foreign-type-size :float) 0))

    (gl:vertex-attrib-pointer 1 3 :float nil
                              (* (cffi:foreign-type-size :float) 8)
                              (* (cffi:foreign-type-size :float) 3))

    (gl:vertex-attrib-pointer 2 2 :float nil
                              (* (cffi:foreign-type-size :float) 8)
                              (* (cffi:foreign-type-size :float) 6))

    (gl:enable-vertex-attrib-array 0)
    (gl:enable-vertex-attrib-array 1)
    (gl:enable-vertex-attrib-array 2)

    (gl:bind-vertex-array 0)
    (gl:bind-buffer :array-buffer 0)

    vao))
