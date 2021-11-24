(in-package :learnopengl)

(defparameter vs nil)
(defparameter vs-src "

#version 130

in vec3 vertexPos;
in vec2 texturePos;

uniform mat4 transform;

out vec4 vertPos;
out vec2 texPos;

void main() {
  vertPos = vec4(vertexPos, 1.0);
  texPos = texturePos;
  gl_Position = transform * vertPos;
}
")


(defparameter fs nil)
(defparameter fs-src "

#version 130

in vec4 vertPos;
in vec2 texPos;
out vec4 fragColor;
uniform sampler2D myTexture;

void main() {
  fragColor = texture(myTexture, texPos) * 1.5 * vertPos ;
}
")

(defparameter program nil)

;;;;

(defparameter vao nil)
(defparameter vbo nil)
(defparameter vertices      ;; coords       ;; texture
  (gl-array :float (vector -0.5 -0.5 1.0    1.0 1.0
                           -0.5  0.5 1.0    1.0 0.0
                            0.5 -0.5 1.0    0.0 0.0
                            0.5  0.5 1.0    0.0 1.0)))
(defparameter ebo nil)
(defparameter indices
  (gl-array :unsigned-int (vector 0 1 2
                                  1 2 3)))

;;;;

(defparameter pyramid-raw
  (with-open-file (stream "~/scratch/pyramid.data" :element-type '(unsigned-byte 8))
    (let ((content (make-array (file-length stream) :element-type (stream-element-type stream))))
      (read-sequence content stream)
      content)))

(defparameter texture nil)

;;;;

(defun render ()
  (gl:clear-color 0.4 0.6 0.6 1.0)
  (gl:clear :color-buffer-bit)
  (gl:bind-texture :texture-2d texture)
  (gl:bind-vertex-array vao)
  (gl:use-program program)
  (gl:uniform-matrix-4fv
   (gl:get-uniform-location program "transform")
   (->> (m* 
     (mtranslation (vec (* 0.5 (sin (al:get-time)))
                        (* 0.5 (cos (al:get-time)))
                        (truncate (sin (al:get-time)))))
     (mrotation +vz+ (-> (al:get-time)
                         sin
                         1+
                         (* 2)
                         (* 60)
                         (coerce 'single-float)
                         degree->radian))
     (mscaling (vec 0.8 0.8 0.8))
     )
    marr))
            
  (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-int) :count 6)
  (al:flip-display)
  (sleep 1/60)
  )

;;;;
(defparameter display nil)

(defun init-opengl ()
  (al:set-target-backbuffer display)

  (setf vs (gl:create-shader :vertex-shader))
  (gl:shader-source vs vs-src)
  (gl:compile-shader vs)

  (setf fs (gl:create-shader :fragment-shader))
  (gl:shader-source fs fs-src)
  (gl:compile-shader fs)

  (setf program (gl:create-program))
  (gl:attach-shader program vs)
  (gl:attach-shader program fs)
  (gl:link-program program)
  (gl:delete-shader vs)
  (gl:delete-shader fs)

  (setf texture (gl:gen-texture))
  (gl:bind-texture :texture-2d texture)
  (gl:tex-image-2d :texture-2d 0 :rgb 100 100 0 :rgba :unsigned-byte pyramid-raw)
  (gl:generate-mipmap :texture-2d)

  (setf vao (gl:create-vertex-array))
  (gl:bind-vertex-array vao)

  (setf ebo (gl:create-buffer))
  (gl:bind-buffer :element-array-buffer ebo)
  (gl:buffer-data :element-array-buffer :static-draw indices)

  (setf vbo (gl:create-buffer))
  (gl:bind-buffer :array-buffer vbo)
  (gl:buffer-data :array-buffer :static-draw vertices)
  (gl:vertex-attrib-pointer 0 3 :float nil (* 5 (cffi:foreign-type-size :float)) 0)
  (gl:enable-vertex-attrib-array 0)
  (gl:vertex-attrib-pointer 1 2 :float nil
                            (* 5 (cffi:foreign-type-size :float))
                            (* 3 (cffi:foreign-type-size :float)))
  (gl:enable-vertex-attrib-array 1)

  (gl:bind-buffer :array-buffer 0)
  (gl:bind-vertex-array 0)
  (gl:bind-buffer :element-array-buffer 0))


(defun main ()
  (unwind-protect
       (progn
         (setf display (init-display))

         (init-opengl)

         (loop
           (with-simple-restart (next-iteration "Continue")
             (funcall 'render))))

    (ignore-errors
     (gl:delete-program program)
     (gl:delete-buffers (list vbo))
     (gl:delete-vertex-arrays (list vao)))
    (destroy-display display)
    (al:uninstall-system)))

;; (defparameter thread (bt:make-thread 'main))
;; (bt:destroy-thread thread)
