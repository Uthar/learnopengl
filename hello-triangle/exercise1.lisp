(in-package :learnopengl)


(defparameter vs-source "
#version 130

in vec3 pos;

out vec4 fragPos;

uniform mat4 thing;

void main() {
  fragPos = thing * vec4(pos, 1.0);
  gl_Position = fragPos;
}
")

(defparameter fs-source "
#version 130

in vec4 fragPos;
out vec4 fragColor;

void main() {
  fragColor = vec4(0.2, 0.6, 0.6, 1.0);
}
")

(defparameter vs nil)
(defparameter fs nil)
(defparameter program nil)

(defparameter vertices
  (gl-array :float (vector -0.5 -0.5 0.0
                           -0.7 -0.2 0.0
                           -0.3 -0.2 0.0
                            0.5  0.5 0.0
                            0.7  0.2 0.0
                            0.3  0.2 0.0
                           )))
(defparameter vao nil)
(defparameter vbo nil)

(defparameter display nil)

(defparameter running t)

(defun render ()
  (gl:clear-color 0.2 0.4 0.6 1.0)
  (gl:clear :color-buffer-bit)

  (gl:use-program program)

  (->>
    (m* 
     (mtranslation (vec 0.0 0.0 0.0))
     (mrotation +vy+ (degree->radian (coerce (* 60 (* 2 (1+ (sin (al:get-time))))) 'single-float)))
     (mrotation +vx+ (degree->radian (coerce (* 60 (* 2 (1+ (cos (al:get-time))))) 'single-float)))
     (mrotation +vz+ (degree->radian (coerce (* 60 (* 2 (1+ (sin (al:get-time))))) 'single-float)))
     ;; (mrotation +vx+ (degree->radian 50.0))
     (mscaling (vec 1.0 1.0 1.0)))
    marr
    (gl:uniform-matrix-4fv (gl:get-uniform-location program "thing")))

  (gl:bind-vertex-array vao)

  ;; Draw both triangles
  (gl:draw-arrays :triangles 0 6)

  (al:flip-display)

  (sleep 1/30))


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
           (error "Error compiling shader")))

         (setf program (gl:create-program))
         (gl:attach-shader program vs)
         (gl:attach-shader program fs)
         (gl:link-program program)
         (format t "~a~%" (gl:get-program-info-log program))
         (unless (gl:get-program program :link-status)
           (error "Error linking shader program"))
         (gl:delete-shader vs)
         (gl:delete-shader fs)

         (setf vao (gl:gen-vertex-array))
         (gl:bind-vertex-array vao)

         (setf vbo (gl:gen-buffer))
         (gl:bind-buffer :array-buffer vbo)
         (gl:buffer-data :array-buffer :static-draw vertices)
         (gl:vertex-attrib-pointer 0 3 :float nil 0 (cffi:null-pointer))
         (gl:enable-vertex-attrib-array 0)
         (gl:bind-buffer :array-buffer 0)
         (gl:bind-vertex-array 0)

         ;; Draw the stuff

    (loop while running do
      (with-simple-restart (next-iteration "Continue with the next iteration of the main loop")
        (funcall 'render)))

         )

    (gl:delete-vertex-arrays (list vao))
    (gl:delete-buffers (list vbo))
    (gl:delete-program program)
    (destroy-display display)
    (al:uninstall-system)))
