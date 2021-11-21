
(in-package :learnopengl)


(defparameter vs-source "
#version 130

in vec3 pos;

void main() {
  gl_Position = vec4(pos.x, pos.y, pos.z, 1.0);
}
")

(defparameter fs-green-source "
#version 130

out vec4 fragColor;

void main() {
  fragColor = vec4(0.2, 0.6, 0.6, 1.0);
}
")

(defparameter fs-yellow-source "
#version 130

out vec4 fragColor;

void main() {
  fragColor = vec4(0.6, 0.6, 0.1, 1.0);
}
")

(defparameter vs nil)

(defparameter fs-green nil)
(defparameter program-green nil)

(defparameter fs-yellow nil)
(defparameter program-yellow nil)

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

         (setf fs-yellow (gl:create-shader :fragment-shader))
         (gl:shader-source fs-yellow fs-yellow-source)
         (gl:compile-shader fs-yellow)
         (format t "~a~%" (gl:get-shader-info-log fs-yellow))
         (unless (gl:get-shader fs-yellow :compile-status)
           (error "Error compiling shader"))

         (setf program-yellow (gl:create-program))
         (gl:attach-shader program-yellow vs)
         (gl:attach-shader program-yellow fs-yellow)
         (gl:link-program program-yellow)
         (format t "~a~%" (gl:get-program-info-log program-yellow))
         (unless (gl:get-program program-yellow :link-status)
           (error "Error linking shader program"))
         (gl:delete-shader fs-yellow)

         (setf fs-green (gl:create-shader :fragment-shader))
         (gl:shader-source fs-green fs-green-source)
         (gl:compile-shader fs-green)
         (format t "~a~%" (gl:get-shader-info-log fs-green))
         (unless (gl:get-shader fs-green :compile-status)
           (error "Error compiling shader"))

         (setf program-green (gl:create-program))
         (gl:attach-shader program-green vs)
         (gl:attach-shader program-green fs-green)
         (gl:link-program program-green)
         (format t "~a~%" (gl:get-program-info-log program-green))
         (unless (gl:get-program program-green :link-status)
           (error "Error linking shader program"))
         (gl:delete-shader fs-green)

         (gl:delete-shader vs)

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


         ;; Draw first triangle
         (gl:use-program program-green)
         (gl:bind-vertex-array vao1)
         (gl:draw-arrays :triangles 0 3)

         ;; Draw second triangle
         (gl:use-program program-yellow)
         (gl:bind-vertex-array vao2)
         (gl:draw-arrays :triangles 0 3)

         (al:flip-display)

         (sleep 5)

         )

    (gl:delete-vertex-arrays (list vao1 vao2))
    (gl:delete-buffers (list vbo1 vbo2))
    (gl:delete-program program-yellow)
    (gl:delete-program program-green)
    (destroy-display display)
    (al:uninstall-system)))
