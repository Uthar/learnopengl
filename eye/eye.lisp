(in-package :learnopengl)


(defparameter vs-source "
#version 130

in vec3 pos;
in vec2 texcoords;

out vec4 fragPos;
out vec2 texPos;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

void main() {
  fragPos = projection * view * model * vec4(pos, 1.0);
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

(defparameter camera-position (vec3 0 0 3))

;; Actually pointing in the reverse direction of where it's targeting
;; (to align with OpenGL being a right-handed coordinate system)
(defparameter camera-direction (vunit (v- camera-position (vec3 0 0 0))))

;; Perpendicular to the world Y axis and the camera Z axis
(defparameter camera-right (vunit (vc (vec3 0 1 0) camera-position)))

;; Perpendicular to both the camera Z and X axes
(defparameter camera-up (vc camera-direction camera-right))

;; We get a 3d coordinate system starting at camera-position

(defun render ()
  (gl:clear-color 0.2 0.4 0.6 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)

  (gl:use-program program)

  (gl:bind-texture :texture-2d pyramid-texture)

  (gl:bind-vertex-array vao)

  (let ((view (mlookat camera-position (v+ camera-position (v- camera-direction)) camera-up))
        (projection (mperspective 45 (/ 800.0 600.0) 0.1 100.0)))

    (gl:uniform-matrix-4fv (gl:get-uniform-location program "view") (marr view))
    (gl:uniform-matrix-4fv (gl:get-uniform-location program "projection") (marr projection)))

  ;;;;

  (flet ((draw-pyramid (translation)

           (let ((model (m* (mtranslation translation)
                            ;; (mrotation +vx+ (degree->radian (coerce (* 30 (al:get-time)) 'single-float)))
                            (mrotation +vy+ (degree->radian (coerce (* 0 (al:get-time)) 'single-float)))
                            ;; (mrotation +vz+ (degree->radian (coerce (* 30 (al:get-time)) 'single-float)))
                            (mscaling (vec3 1.0 0.8 1.0)))))

             (gl:uniform-matrix-4fv (gl:get-uniform-location program "model") (marr model)))

           (gl:draw-elements :triangles
                             (gl:make-null-gl-array :unsigned-int)
                             :count (slot-value indices 'gl::size))))

    (draw-pyramid (vec3 0.0 0.0 0.0))
    (draw-pyramid (vec3 2.0 0.5 -3.0))
    (draw-pyramid (vec3 -2.0 1.2 -2.0))
    (draw-pyramid (vec3 -2.2 -1.2 -6.0))
    (draw-pyramid (vec3 -2.7 -2.7 -2.0))
    (draw-pyramid (vec3 1.6 3.3 -3.7))
    (draw-pyramid (vec3 2.0 2.7 -7.0))
    (draw-pyramid (vec3 2.0 -2.0 -4.0))
    (draw-pyramid (vec3 0.5 -1.0 -2.0))
    (draw-pyramid (vec3 -1.9 -0.2 -2.5))
    (draw-pyramid (vec3 -1.5 -2.1 -4.0))
    (draw-pyramid (vec3 -1.2 2.1 -5.0))
    )

  ;;;;

  (al:flip-display)

  (sleep 1/60))

(defparameter event-queue nil)

(defgeneric handle-event (event-type event))

(defparameter camera-speed 0.25)

(defparameter mouse-enabled t)

(defparameter first-mouse nil)

(defmethod handle-event ((event-type (eql :key-char)) event)
  (let ((keycode (cffi:foreign-slot-value event '(:struct al:keyboard-event) 'al::keycode)))
    (case keycode
      (:up    (nv+ camera-position (v* (v- camera-direction) camera-speed)))
      (:down  (nv- camera-position (v* (v- camera-direction) camera-speed)))
      (:left  (nv+ camera-position (v* (vunit (vc camera-direction (vec3 0 1 0))) camera-speed)))
      (:right (nv- camera-position (v* (vunit (vc camera-direction (vec3 0 1 0))) camera-speed)))
      (:escape (progn
                 (al:set-mouse-xy display 400 300)
                 (setf first-mouse t
                       mouse-enabled (not mouse-enabled)
                       )
                 )))))

(defparameter yaw 0.0)
(defparameter pitch 0.0)

(defmethod handle-event ((event-type (eql :mouse-axis)) event)
  (when mouse-enabled
    (let* ((x (if first-mouse 0 (cffi:foreign-slot-value event '(:struct al:mouse-event) 'al::dx)))
           (y (if first-mouse 0 (cffi:foreign-slot-value event '(:struct al:mouse-event) 'al::dy)))
           (sensitivity 0.3)
           (offset-x (* sensitivity x))
           (offset-y (* sensitivity y))
           )
      (incf yaw offset-x)
      (setf pitch (alexandria:clamp (- pitch offset-y) -89.0 89.0))
      (setf camera-direction (v- (vunit (vec3 (* (cos (degree->radian yaw)) (cos (degree->radian pitch)))
                                              (sin (degree->radian pitch))
                                              (* (sin (degree->radian yaw)) (cos (degree->radian pitch)))))))
      (al:set-mouse-xy display 400 300)
      (setf first-mouse nil))))


(defmethod handle-event ((event-type t) event)
  ;; (format t "Unknown event ~a: ~a~%" event-type event))
  )

(defun handle-input ()
  (cffi:with-foreign-object (event '(:union al:event))
    (loop while (al:get-next-event event-queue event)
          do (handle-event
              (cffi:foreign-slot-value event '(:struct al:any-event) 'type)
              event))))

(defun run-example ()

  (unwind-protect
       (progn
         (setf display (init-display))
         (al:set-target-backbuffer display)
         (setf event-queue (al:create-event-queue))
         (al:install-keyboard)
         (al:install-mouse)
         (al:register-event-source event-queue (al:get-mouse-event-source))
         (al:register-event-source event-queue (al:get-keyboard-event-source))
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
             (funcall 'handle-input)
             (funcall 'render))))

    (gl:delete-vertex-arrays (list vao))
    (gl:delete-buffers (list vbo ebo))
    (gl:delete-program program)
    (destroy-display display)
    (al:uninstall-system)))


;; (defparameter thread (bt:make-thread 'run-example))
;; (bt:destroy-thread thread)
