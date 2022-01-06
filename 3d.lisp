(in-package :learnopengl)

(defun cube ()
  (let* ((vao (gl:create-vertex-array))
         (vbo (gl:create-buffer))
         ;; (ebo (gl:create-buffer))
         (vertices
           (vector -0.5 -0.5 -0.5  0.0  0.0 -1.0
                    0.5 -0.5 -0.5  0.0  0.0 -1.0
                    0.5  0.5 -0.5  0.0  0.0 -1.0
                    0.5  0.5 -0.5  0.0  0.0 -1.0
                   -0.5  0.5 -0.5  0.0  0.0 -1.0
                   -0.5 -0.5 -0.5  0.0  0.0 -1.0

                   -0.5 -0.5  0.5  0.0  0.0 1.0
                    0.5 -0.5  0.5  0.0  0.0 1.0
                    0.5  0.5  0.5  0.0  0.0 1.0
                    0.5  0.5  0.5  0.0  0.0 1.0
                   -0.5  0.5  0.5  0.0  0.0 1.0
                   -0.5 -0.5  0.5  0.0  0.0 1.0

                   -0.5  0.5  0.5 -1.0  0.0  0.0
                   -0.5  0.5 -0.5 -1.0  0.0  0.0
                   -0.5 -0.5 -0.5 -1.0  0.0  0.0
                   -0.5 -0.5 -0.5 -1.0  0.0  0.0
                   -0.5 -0.5  0.5 -1.0  0.0  0.0
                   -0.5  0.5  0.5 -1.0  0.0  0.0

                    0.5  0.5  0.5  1.0  0.0  0.0
                    0.5  0.5 -0.5  1.0  0.0  0.0
                    0.5 -0.5 -0.5  1.0  0.0  0.0
                    0.5 -0.5 -0.5  1.0  0.0  0.0
                    0.5 -0.5  0.5  1.0  0.0  0.0
                    0.5  0.5  0.5  1.0  0.0  0.0

                   -0.5 -0.5 -0.5  0.0 -1.0  0.0
                    0.5 -0.5 -0.5  0.0 -1.0  0.0
                    0.5 -0.5  0.5  0.0 -1.0  0.0
                    0.5 -0.5  0.5  0.0 -1.0  0.0
                   -0.5 -0.5  0.5  0.0 -1.0  0.0
                   -0.5 -0.5 -0.5  0.0 -1.0  0.0

                   -0.5  0.5 -0.5  0.0  1.0  0.0
                    0.5  0.5 -0.5  0.0  1.0  0.0
                    0.5  0.5  0.5  0.0  1.0  0.0
                    0.5  0.5  0.5  0.0  1.0  0.0
                   -0.5  0.5  0.5  0.0  1.0  0.0
                   -0.5  0.5 -0.5  0.0  1.0  0.0)))

         ;; (vertices (vector  1  1  1   0 0  1
         ;;                    1  1 -1   0 0 -1
         ;;                    1 -1  1
         ;;                    1 -1 -1
         ;;                   -1  1  1
         ;;                   -1  1 -1
         ;;                   -1 -1  1
         ;;                   -1 -1 -1))
         ;; (mvertices (matn 8 3 vertices))
         ;; (indices (vector 0 2 6 ;; front wall
         ;;                  0 4 6

         ;;                  1 3 7 ;; back wall
         ;;                  1 5 7

         ;;                  1 0 2 ;; right wall
         ;;                  1 3 2

         ;;                  5 4 6 ;; left wall
         ;;                  5 7 6

         ;;                  5 1 0 ;; top wall
         ;;                  5 4 0

         ;;                  7 3 2 ;; bottom wall
         ;;                  7 6 2)))

    (gl:bind-vertex-array vao)

    (gl:bind-buffer :array-buffer vbo)
    ;; (gl:buffer-data :array-buffer :static-draw (gl-array :float (->> mvertices (m* side) marr)))
    (gl:buffer-data :array-buffer :static-draw (gl-array :float vertices))
    (gl:vertex-attrib-pointer 0 3 :float nil
                              (* (cffi:foreign-type-size :float) 6)
                              (* (cffi:foreign-type-size :float) 0))
    (gl:vertex-attrib-pointer 1 3 :float nil
                              (* (cffi:foreign-type-size :float) 6)
                              (* (cffi:foreign-type-size :float) 3))
    (gl:enable-vertex-attrib-array 0)
    (gl:enable-vertex-attrib-array 1)

    ;; (gl:bind-buffer :element-array-buffer ebo)
    ;; (gl:buffer-data :element-array-buffer :static-draw (gl-array :unsigned-int indices))

    (gl:bind-vertex-array 0)
    (gl:bind-buffer :array-buffer 0)
    ;; (gl:bind-buffer :element-array-buffer 0)

    vao))
