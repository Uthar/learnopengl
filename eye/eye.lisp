(in-package :learnopengl)


(defparameter vs-source "
#version 130

in vec3 pos;
in vec2 texcoords;

out vec4 fragPos;
out vec2 texPos;

uniform mat4 thing;

void main() {
  fragPos = thing * vec4(pos, 1.0);
  texPos = texcoords;
  gl_Position = fragPos;
}
")

(defparameter fs-source "
#version 130

in vec4 fragPos;
in vec2 texPos;

uniform sampler2D tex;

out vec4 fragColor;

void main() {
  fragColor = texture(tex, texPos);
}
")

(defparameter vs nil)
(defparameter fs nil)
(defparameter program nil)

(defparameter vertices nil)
(defparameter indices nil)

(defparameter vao nil)
(defparameter vbo nil)
(defparameter ebo nil)

(defparameter display nil)

(defparameter running t)

(defparameter pyramid-raw
  (with-open-file (stream "pyramid.data" :element-type '(unsigned-byte 8))
    (let ((content (make-array (file-length stream) :element-type (stream-element-type stream))))
      (read-sequence content stream)
      content)))

(defparameter pyramid-texture nil)

(defun render ()
  (gl:clear-color 0.2 0.4 0.6 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)

  (gl:use-program program)

  (->>
    (m* 
     (mtranslation (vec 0.2
                        (- (* 0.25 (sin (al:get-time))) 0.3)
                        0.0))
     (mrotation +vx+ (degree->radian (coerce (* 30 (sin (al:get-time))) 'single-float)))
     (mrotation +vy+ (degree->radian (* 25 (coerce (al:get-time) 'single-float))))
     (mrotation +vz+ (degree->radian -15.0))
     (mscaling (vec 1.0 0.8 1.0)))
    marr
    (gl:uniform-matrix-4fv (gl:get-uniform-location program "thing")))

  (gl:bind-texture :texture-2d pyramid-texture)

  (gl:bind-vertex-array vao)

  ;; Draw both triangles
  (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-int) :count 20)
  ;; (gl:draw-arrays :triangles 0 12)

  (al:flip-display)

  (sleep 1/60))

(defun run-example ()

  (unwind-protect
       (progn
         (setf display (init-display))
         (al:set-target-backbuffer display)
         (gl:enable :depth-test)

         (setf vertices
               (gl-array :float (vector 0.0  1.0  0.0    0.5 1.0
                                        0.5  0.0 -0.5    0.0 0.0
                                        -0.5  0.0 -0.5    1.0 0.0
                                        -0.5  0.0  0.5    0.1 0.0
                                        0.5  0.0  0.5    0.0 1.0
                                        )))


         (setf indices
               (gl-array :unsigned-int (vector 0 1 2
                                               0 2 3
                                               0 3 4
                                               0 4 1
                                               2 4 1
                                               2 4 3
                                               )))

         (setf pyramid-texture (gl:gen-texture))
         (gl:bind-texture :texture-2d pyramid-texture)
         (gl:tex-image-2d :texture-2d 0 :rgb 100 100 0 :rgba :unsigned-byte pyramid-raw)
         (gl:generate-mipmap :texture-2d)


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

         (setf vao (gl:gen-vertex-array))
         (gl:bind-vertex-array vao)

         (setf ebo (gl:gen-buffer))
         (gl:bind-buffer :element-array-buffer ebo)
         (gl:buffer-data :element-array-buffer :static-draw indices)

         (setf vbo (gl:gen-buffer))
         (gl:bind-buffer :array-buffer vbo)
         (gl:buffer-data :array-buffer :static-draw vertices)
         (gl:vertex-attrib-pointer 0 3 :float nil (* 5 (cffi:foreign-type-size :float)) 0)
         (gl:vertex-attrib-pointer 1 2 :float nil
                                   (* 5 (cffi:foreign-type-size :float))
                                   (* 3 (cffi:foreign-type-size :float)))
         (gl:enable-vertex-attrib-array 0)
         (gl:enable-vertex-attrib-array 1)
         (gl:bind-vertex-array 0)
         (gl:bind-buffer :array-buffer 0)
         (gl:bind-buffer :element-array-buffer 0)

         ;; Draw the stuff
         (loop while running do
           (with-simple-restart (next-iteration "Continue")
             (funcall 'render))))

    (gl:delete-vertex-arrays (list vao))
    (gl:delete-buffers (list vbo))
    (gl:delete-program program)
    (destroy-display display)
    (al:uninstall-system)))


;; (defparameter thread (bt:make-thread 'run-example))
;; (bt:destroy-thread thread)
