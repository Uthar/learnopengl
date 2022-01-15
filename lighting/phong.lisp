(in-package :learnopengl)

(defparameter light-pos (vec3 1.6 -0.0 -4.7))
(defparameter light-color (vector 0.0 7.0 0.0))
(defparameter light-2-pos (vec3 1.6 -0.0 -4.7))
(defparameter light-2-color (vector 7.0 0.0 0.0))

;; TODO optimizations:
;; - cache uniform locations
;; - disable cl-opengl error checking
;; -
(defun draw-box (x y z)
  "Draw a big box at (x, y, z) world coordinates"
  (let ((model (m*
                (mtranslation (vec3 x y z))
                (mrotation +vy+ (- (degree->radian (coerce (* 20 (al:get-time)) 'single-float))))
                ;; (mrotation +vx+ (degree->radian -65.0))
                ;; (mrotation +vz+ (degree->radian -45.0))
                (mscaling (vec3 1.7 1.7 1.7))
                )))
    (gl:uniform-matrix-4fv (gl:get-uniform-location big-cube-shader "model")
                           (marr model))
    (gl:uniform-matrix-3fv (gl:get-uniform-location big-cube-shader "normal")
                           (marr (mblock (mtranspose (minv model)) 0 0 3 3))))
  (gl:bind-vertex-array big-cube)
  (gl:draw-arrays :triangles 0 36))

(defun render ()
  (gl:clear-color 0.1 0.1 0.1 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)

  (gl:use-program light-cube-shader)
  (gl:bind-vertex-array light-cube)

  (setf light-pos (vec3
                   (* 1.2 (sin (* 1.0 (al:get-time))))
                   ;; 0.75
                   (* 3.6 (sin (* 1.0 (al:get-time))))
                   ;; 0.6
                   (- (* 3.2 (cos (* 1.0 (al:get-time)))) 6)
                   ;; 1.0
                   ))
  (gl:uniform-matrix-4fv (gl:get-uniform-location light-cube-shader "model")
                         (marr (m*
                                ;; (mrotation +vx+ (degree->radian 15.0))
                                ;; (mrotation +vy+ (degree->radian -10.0))
                                ;; (mtranslation (vec3 1.0 1.0 0.0))
                                (mtranslation light-pos)
                                (mscaling (vec3 0.2 0.2 0.2)))))
  (gl:uniformfv (gl:get-uniform-location light-cube-shader "color") light-color)
  (gl:draw-arrays :triangles 0 36)

  (setf light-2-pos (vec3
                   (* -1.2 (sin (* 1.0 (al:get-time))))
                   ;; 0.75
                   (* -3.6 (sin (* 1.0 (al:get-time))))
                   ;; 0.6
                   (- (* 3.2 (cos (* 1.0 (al:get-time)))) 6)
                   ;; 1.0
                   ))
  (gl:uniform-matrix-4fv (gl:get-uniform-location light-cube-shader "model")
                         (marr (m*
                                ;; (mrotation +vx+ (degree->radian 15.0))
                                ;; (mrotation +vy+ (degree->radian -10.0))
                                ;; (mtranslation (vec3 1.0 1.0 0.0))
                                (mtranslation light-2-pos)
                                (mscaling (vec3 0.2 0.2 0.2)))))
  (gl:uniformfv (gl:get-uniform-location light-cube-shader "color") light-2-color)
  (gl:draw-arrays :triangles 0 36)

  (gl:bind-vertex-array 0)
  (gl:use-program 0)

  (gl:use-program big-cube-shader)

  (gl:uniformfv (gl:get-uniform-location big-cube-shader "viewPos") (varr3 (slot-value camera 'position)))

  (gl:active-texture :texture0)
  (gl:bind-texture :texture-2d container-diffuse-map)
  (gl:active-texture :texture1)
  (gl:bind-texture :texture-2d container-specular-map)
  (gl:active-texture :texture2)
  (gl:bind-texture :texture-2d container-emission-map)

  (gl:uniformi (gl:get-uniform-location big-cube-shader "material.diffuse") 0)
  (gl:uniformi (gl:get-uniform-location big-cube-shader "material.specular") 1)
  (gl:uniformi (gl:get-uniform-location big-cube-shader "material.emission") 2)
  (gl:uniformf  (gl:get-uniform-location big-cube-shader "material.shininess") 32.0)

  (gl:uniformfv (gl:get-uniform-location big-cube-shader "dirLight.direction") (vector -0.3 -1.0 -1.0))
  (gl:uniformfv (gl:get-uniform-location big-cube-shader "dirLight.ambient") (vector 0.2 0.2 0.2))
  (gl:uniformfv (gl:get-uniform-location big-cube-shader "dirLight.diffuse") (vector 1.0 1.0 1.0))
  (gl:uniformfv (gl:get-uniform-location big-cube-shader "dirLight.specular") (vector 0.5 0.5 0.5))

  (gl:uniformfv (gl:get-uniform-location big-cube-shader "pointLights[0].position") (varr3 light-pos))
  (gl:uniformf  (gl:get-uniform-location big-cube-shader "pointLights[0].constant")  1.0)
  (gl:uniformf  (gl:get-uniform-location big-cube-shader "pointLights[0].linear")    0.220)
  (gl:uniformf  (gl:get-uniform-location big-cube-shader "pointLights[0].quadratic") 0.20)
  (gl:uniformfv (gl:get-uniform-location big-cube-shader "pointLights[0].ambient") (vector 0.2 0.2 0.2))
  (gl:uniformfv (gl:get-uniform-location big-cube-shader "pointLights[0].diffuse") light-color)
  (gl:uniformfv (gl:get-uniform-location big-cube-shader "pointLights[0].specular") (vector 0.5 0.5 0.5))

  (gl:uniformfv (gl:get-uniform-location big-cube-shader "pointLights[1].position") (varr3 light-2-pos))
  (gl:uniformf  (gl:get-uniform-location big-cube-shader "pointLights[1].constant")  1.0)
  (gl:uniformf  (gl:get-uniform-location big-cube-shader "pointLights[1].linear")    0.220)
  (gl:uniformf  (gl:get-uniform-location big-cube-shader "pointLights[1].quadratic") 0.20)
  (gl:uniformfv (gl:get-uniform-location big-cube-shader "pointLights[1].ambient") (vector 0.2 0.2 0.2))
  (gl:uniformfv (gl:get-uniform-location big-cube-shader "pointLights[1].diffuse") light-2-color)
  (gl:uniformfv (gl:get-uniform-location big-cube-shader "pointLights[1].specular") (vector 0.5 0.5 0.5))

  (gl:uniformi  (gl:get-uniform-location big-cube-shader "numPointLights") 2)

  (gl:uniformfv (gl:get-uniform-location big-cube-shader "spotLights[0].position") (varr3 (slot-value camera 'position)))
  (gl:uniformfv (gl:get-uniform-location big-cube-shader "spotLights[0].direction") (varr3 (slot-value camera 'direction)))
  (gl:uniformf  (gl:get-uniform-location big-cube-shader "spotLights[0].cutoff") (cos (degree->radian 8.5)))
  (gl:uniformf  (gl:get-uniform-location big-cube-shader "spotLights[0].outer") (cos (degree->radian 9.5)))
  (gl:uniformfv (gl:get-uniform-location big-cube-shader "spotLights[0].ambient") (vector 0.2 0.2 0.2))
  (gl:uniformfv (gl:get-uniform-location big-cube-shader "spotLights[0].diffuse") (vector 1.0 1.0 1.0))
  (gl:uniformfv (gl:get-uniform-location big-cube-shader "spotLights[0].specular") (vector 0.5 0.5 0.5))

  (if flashlight-active-p
      (gl:uniformi  (gl:get-uniform-location big-cube-shader "numSpotLights") 1)
      (gl:uniformi  (gl:get-uniform-location big-cube-shader "numSpotLights") 0))


  (gl:uniformf (gl:get-uniform-location big-cube-shader "time") (al:get-time))

  (draw-box -0.5 0 -6.2)
  (draw-box -5.5 1.9 -12.2)
  (draw-box -3.4 -2 -7.7)
  (draw-box 3.5 3.6 -20.0)
  (draw-box 0.2 4.9 -14.5)
  (draw-box -3.1 3.9 -10.2)
  (draw-box 1.5 -2.9 -8.8)

  (gl:bind-vertex-array 0)
  (gl:use-program 0)



  (gl:use-program backpack-shader)

  (gl:uniformf  (gl:get-uniform-location backpack-shader "material.shininess") 32.0)

  (gl:uniformfv (gl:get-uniform-location backpack-shader "dirLight.direction") (vector -0.3 -1.0 -1.0))
  (gl:uniformfv (gl:get-uniform-location backpack-shader "dirLight.ambient") (vector 0.2 0.2 0.2))
  (gl:uniformfv (gl:get-uniform-location backpack-shader "dirLight.diffuse") (vector 1.0 1.0 1.0))
  (gl:uniformfv (gl:get-uniform-location backpack-shader "dirLight.specular") (vector 0.5 0.5 0.5))

  (gl:uniformfv (gl:get-uniform-location backpack-shader "pointLights[0].position") (varr3 light-pos))
  (gl:uniformf  (gl:get-uniform-location backpack-shader "pointLights[0].constant")  1.0)
  (gl:uniformf  (gl:get-uniform-location backpack-shader "pointLights[0].linear")    0.220)
  (gl:uniformf  (gl:get-uniform-location backpack-shader "pointLights[0].quadratic") 0.20)
  (gl:uniformfv (gl:get-uniform-location backpack-shader "pointLights[0].ambient") (vector 0.2 0.2 0.2))
  (gl:uniformfv (gl:get-uniform-location backpack-shader "pointLights[0].diffuse") light-color)
  (gl:uniformfv (gl:get-uniform-location backpack-shader "pointLights[0].specular") (vector 0.5 0.5 0.5))

  (gl:uniformfv (gl:get-uniform-location backpack-shader "pointLights[1].position") (varr3 light-2-pos))
  (gl:uniformf  (gl:get-uniform-location backpack-shader "pointLights[1].constant")  1.0)
  (gl:uniformf  (gl:get-uniform-location backpack-shader "pointLights[1].linear")    0.220)
  (gl:uniformf  (gl:get-uniform-location backpack-shader "pointLights[1].quadratic") 0.20)
  (gl:uniformfv (gl:get-uniform-location backpack-shader "pointLights[1].ambient") (vector 0.2 0.2 0.2))
  (gl:uniformfv (gl:get-uniform-location backpack-shader "pointLights[1].diffuse") light-2-color)
  (gl:uniformfv (gl:get-uniform-location backpack-shader "pointLights[1].specular") (vector 0.5 0.5 0.5))

  (gl:uniformi  (gl:get-uniform-location backpack-shader "numPointLights") 2)

  (gl:uniformfv (gl:get-uniform-location backpack-shader "spotLights[0].position") (varr3 (slot-value camera 'position)))
  (gl:uniformfv (gl:get-uniform-location backpack-shader "spotLights[0].direction") (varr3 (slot-value camera 'direction)))
  (gl:uniformf  (gl:get-uniform-location backpack-shader "spotLights[0].cutoff") (cos (degree->radian 8.5)))
  (gl:uniformf  (gl:get-uniform-location backpack-shader "spotLights[0].outer") (cos (degree->radian 9.5)))
  (gl:uniformfv (gl:get-uniform-location backpack-shader "spotLights[0].ambient") (vector 0.2 0.2 0.2))
  (gl:uniformfv (gl:get-uniform-location backpack-shader "spotLights[0].diffuse") (vector 1.0 1.0 1.0))
  (gl:uniformfv (gl:get-uniform-location backpack-shader "spotLights[0].specular") (vector 0.5 0.5 0.5))

  (if flashlight-active-p
      (gl:uniformi  (gl:get-uniform-location backpack-shader "numSpotLights") 1)
      (gl:uniformi  (gl:get-uniform-location backpack-shader "numSpotLights") 0))

  (gl:uniformf (gl:get-uniform-location backpack-shader "time") (al:get-time))

  (let ((model (m*
                (mtranslation (vec3 0 -5 -15))
                (mrotation +vy+ (- (degree->radian (coerce (* 20 (al:get-time)) 'single-float))))
                ;; (mrotation +vx+ (degree->radian -90.0))
                ;; (mrotation +vz+ (degree->radian -45.0))
                (mscaling (vec3 0.5 0.5 0.5))
                )))
    (gl:uniform-matrix-4fv (gl:get-uniform-location backpack-shader "model")
                           (marr model))
    (gl:uniform-matrix-3fv (gl:get-uniform-location backpack-shader "normal")
                           (marr (mblock (mtranspose (minv model)) 0 0 3 3))))
  ;; (draw old-car backpack-shader)
  ;; (draw backpack backpack-shader)
  (draw nanosuit backpack-shader)

  (let ((model (m*
                (mtranslation (vec3 -5 -5 -15))
                (mrotation +vy+ (- (degree->radian (coerce (* 20 (al:get-time)) 'single-float))))
                ;; (mrotation +vx+ (degree->radian -90.0))
                ;; (mrotation +vz+ (degree->radian -45.0))
                (mscaling (vec3 0.5 0.5 0.5))
                )))
    (gl:uniform-matrix-4fv (gl:get-uniform-location backpack-shader "model")
                           (marr model))
    (gl:uniform-matrix-3fv (gl:get-uniform-location backpack-shader "normal")
                           (marr (mblock (mtranspose (minv model)) 0 0 3 3))))
  (draw nanosuit backpack-shader)

  (let ((model (m*
                (mtranslation (vec3 +5 -5 -15))
                (mrotation +vy+ (- (degree->radian (coerce (* 20 (al:get-time)) 'single-float))))
                ;; (mrotation +vx+ (degree->radian -90.0))
                ;; (mrotation +vz+ (degree->radian -45.0))
                (mscaling (vec3 0.5 0.5 0.5))
                )))
    (gl:uniform-matrix-4fv (gl:get-uniform-location backpack-shader "model")
                           (marr model))
    (gl:uniform-matrix-3fv (gl:get-uniform-location backpack-shader "normal")
                           (marr (mblock (mtranspose (minv model)) 0 0 3 3))))
  (draw nanosuit backpack-shader)

  (gl:use-program 0)

  (al:flip-display))

(defparameter width 800)
(defparameter height 600)

(defun init ()
  (al:set-target-backbuffer display)
  (gl:viewport 0 0 width height)
  (gl:enable :depth-test)

  (ignore-errors
   (gl:delete-program light-cube-shader)
   (gl:delete-program big-cube-shader)
   (gl:delete-program backpack-shader)
   (gl:delete-vertex-arrays (list light-cube))
   (gl:delete-buffers (list light-cube))
   (gl:delete-vertex-arrays (list big-cube))
   (gl:delete-buffers (list big-cube))
   (gl:delete-texture container-diffuse-map)
   (gl:delete-texture container-specular-map)
   (gl:delete-texture container-emission-map))

  (defparameter light-cube-shader (shader "lighting/vs.vert" "lighting/light-cube.frag"))
  (defparameter big-cube-shader (shader "lighting/vs.vert" "lighting/big-cube.frag"))
  (defparameter backpack-shader (shader "lighting/vs.vert" "lighting/backpack.frag"))
  (defparameter shaders (list light-cube-shader big-cube-shader backpack-shader))
  (defparameter container-diffuse-map (texture "lighting/container2.png"))
  (defparameter container-specular-map (texture "lighting/container2_specular.png"))
  (defparameter container-emission-map (texture "lighting/matrix.jpg"))
  (defparameter big-cube (cube))
  (defparameter light-cube (cube))
  (defparameter camera (camera 0 0 3))
  (defparameter mouse-enabled nil)
  (defvar backpack (make-instance 'model :path "assets/backpack/backpack.obj"))
  (defvar nanosuit (make-instance 'model :path "assets/nanosuit/nanosuit.obj"))

  (let ((view (view-matrix camera))
        (projection (mperspective (slot-value camera 'zoom) (/ width height) 0.1 100.0)))
    (dolist (s shaders)
      (gl:use-program s)
      (gl:uniform-matrix-4fv (gl:get-uniform-location s "view") (marr view))
      (gl:uniform-matrix-4fv (gl:get-uniform-location s "projection") (marr projection))
      (gl:use-program 0))))

(defun process-input ()
  (cffi:with-foreign-object (event '(:union al:event))
    (loop while (al:get-next-event event-queue event)
          do (handle-event
              (cffi:foreign-slot-value event '(:struct al:any-event) 'type)
              event))))

(defparameter walk-speed 0.20)

(defparameter *walking* nil)

(defun walk* (camera &key (x 0.0) (y 0.0) (z 0.0))
  (let ((view-matrix (walk camera :x x :y y :z z)))
    (dolist (s shaders)
      (gl:use-program s)
      (gl:uniform-matrix-4fv (gl:get-uniform-location s "view") (marr view-matrix)))))

(defparameter flashlight-active-p t)

(defmethod handle-event ((event-type (eql :key-down)) event)
  (let ((keycode (cffi:foreign-slot-value event '(:struct al:keyboard-event) 'al::keycode)))
    (case keycode
      (:w (pushnew :front *walking*))
      (:s (pushnew :back *walking*))
      (:d (pushnew :left *walking*))
      (:a (pushnew :right *walking*))
      (:space (pushnew :up *walking*))
      (:v     (pushnew :down *walking*)))))

(defmethod handle-event ((event-type (eql :key-up)) event)
  (let ((keycode (cffi:foreign-slot-value event '(:struct al:keyboard-event) 'al::keycode)))
    (case keycode
      (:w (removef *walking* :front))
      (:s (removef *walking* :back))
      (:d (removef *walking* :left))
      (:a (removef *walking* :right))
      (:space (removef *walking* :up))
      (:v     (removef *walking* :down)))))

;; Prevent keep walking after mouse out of window
(defmethod handle-event ((event-type (eql :display-switch-out)) event)
  (setf *walking* nil))

(defmethod handle-event ((event-type (eql :key-char)) event)
  (let ((keycode (cffi:foreign-slot-value event '(:struct al:keyboard-event) 'al::keycode)))
    (case keycode
      (:f     (setf flashlight-active-p (not flashlight-active-p)))
      (:escape (progn
                 (al:set-mouse-xy display (/ width 2) (/ height 2))
                 (setf mouse-enabled (not mouse-enabled)))))))

;; FIXME make walking speed framerate independent
(defun simulate-game ()
  (when (member :front *walking*) (walk* camera :z (- walk-speed)))
  (when (member :back  *walking*) (walk* camera :z (+ walk-speed)))
  (when (member :left  *walking*) (walk* camera :x (- walk-speed)))
  (when (member :right *walking*) (walk* camera :x (+ walk-speed)))
  (when (member :up    *walking*) (walk* camera :y (+ walk-speed)))
  (when (member :down  *walking*) (walk* camera :y (- walk-speed))))

(defmethod handle-event ((event-type (eql :mouse-axis)) event)
  (when mouse-enabled
    (let* ((x (cffi:foreign-slot-value event '(:struct al:mouse-event) 'al::dx))
           (y (cffi:foreign-slot-value event '(:struct al:mouse-event) 'al::dy))
           (sensitivity 0.15)
           (x-offset (* sensitivity x))
           (y-offset (* sensitivity y))
           (view-matrix (look-around camera x-offset y-offset)))
      (dolist (s shaders)
        (gl:use-program s)
        (gl:uniform-matrix-4fv (gl:get-uniform-location s "view") (marr view-matrix)))
      (al:set-mouse-xy display (/ width 2) (/ height 2)))))

(defmethod handle-event ((event-type (eql :display-close)) event)
  (format t "Display close event ~a: ~a~%" event-type event)
  (error "Display closed"))

(defmethod handle-event ((event-type t) event)
  ;; (format t "Unknown event type ~a: ~a~%" event-type event)
  )

(defun game-sleep ()
  (sleep 1/60))

(defun mainloop ()
  (init)
  (loop
    (with-simple-restart (next-iteration "Continue")
      (process-input)
      (simulate-game)
      (render)
      (game-sleep))))

(defvar mainloop-thread nil)

(defun start ()
  (create-context)
  (reset))

(defun reset ()
  (ignore-errors (bt:destroy-thread mainloop-thread))
  (setf mainloop-thread (bt:make-thread #'mainloop)))

(defun phong ()
  (start)
  (bt:join-thread mainloop-thread))
