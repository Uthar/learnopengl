(in-package :learnopengl)


(defparameter vs-source "
#version 130

in vec3 pos;

void main() {
  gl_Position = vec4(pos.x, pos.y, pos.z, 1.0);
}
")

(defparameter fs-source "
#version 130

out vec4 fragColor;

void main() {
  fragColor = vec4(0.2, 0.6, 0.6, 1.0);
}
")

(defparameter vs nil)
(defparameter fs nil)
(defparameter program nil)

(defparameter vertices1
  (gl-array :float (vector -0.5 -0.5 0.0
                           -0.7 -0.2 0.0
                           -0.3 -0.2 0.0)))
(defparameter vao1 nil)
(defparameter vbo1 nil)

(defparameter vertices2
  (gl-array :float (vector 0.5 0.5 0.0
                           0.7 0.2 0.0
                           0.3 0.2 0.0)))
(defparameter vao2 nil)
(defparameter vbo2 nil)

(defparameter display nil)

(defun run-example ()

  (unwind-protect
       (progn
         (setf display (init-display))
         (al:set-target-backbuffer display)

         (setf vs (gl:create-shader :vertex-shader))
         (gl:shader-source vs vs-source)
         (gl:compile-shader vs)
         (format t "~a~%" (gl:get-shader-info-log vs))
         (unless (gl:get-shader vs :compile-status)
           (error "Error compiling shader"))

         (setf fs (gl:create-shader :fragment-shader))
         (gl:shader-source fs fs-source)
         (gl:compile-shader fs)
         (format t "~a~%" (gl:get-shader-info-log fs))
         (unless (gl:get-shader fs :compile-status)
           (error "Error compiling shader"))

         (setf program (gl:create-program))
         (gl:attach-shader program vs)
         (gl:attach-shader program fs)
         (gl:link-program program)
         (format t "~a~%" (gl:get-program-info-log program))
         (unless (gl:get-program program :link-status)
           (error "Error linking shader program"))
         (gl:delete-shader vs)
         (gl:delete-shader fs)

         (setf vao1 (gl:gen-vertex-array))
         (gl:bind-vertex-array vao1)

         (setf vbo1 (gl:gen-buffer))
         (gl:bind-buffer :array-buffer vbo1)
         (gl:buffer-data :array-buffer :static-draw vertices1)
         (gl:vertex-attrib-pointer 0 3 :float nil 0 (cffi:null-pointer))
         (gl:enable-vertex-attrib-array 0)
         (gl:bind-buffer :array-buffer 0)
         (gl:bind-vertex-array 0)

         (setf vao2 (gl:gen-vertex-array))
         (gl:bind-vertex-array vao2)

         (setf vbo2 (gl:gen-buffer))
         (gl:bind-buffer :array-buffer vbo2)
         (gl:buffer-data :array-buffer :static-draw vertices2)
         (gl:vertex-attrib-pointer 0 3 :float nil 0 (cffi:null-pointer))
         (gl:enable-vertex-attrib-array 0)
         (gl:bind-buffer :array-buffer 0)
         (gl:bind-vertex-array 0)

         ;; Draw the stuff

         (gl:clear-color 0.2 0.4 0.6 1.0)
         (gl:clear :color-buffer-bit)

         (gl:use-program program)

         ;; Draw first triangle
         (gl:bind-vertex-array vao1)
         (gl:draw-arrays :triangles 0 3)

         ;; Draw second triangle
         (gl:bind-vertex-array vao2)
         (gl:draw-arrays :triangles 0 3)

         (al:flip-display)

         (sleep 5)

         )

    (gl:delete-vertex-arrays (list vao1 vao2))
    (gl:delete-buffers (list vbo1 vbo2))
    (gl:delete-program program)
    (destroy-display display)
    (al:uninstall-system)))
