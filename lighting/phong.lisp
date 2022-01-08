(in-package :learnopengl)

(defparameter light-pos (vec3 0.6 0.4 0.7))
(defparameter camera-position (vec3 0 0 3))
(defparameter camera-direction (vec3 0 0 2))
(defparameter camera-up +vy+)

(defun render ()
  (gl:clear-color 0.1 0.1 0.1 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)

  (gl:use-program light-cube-shader)
  ;; (setf light-pos (vec3
  ;;                  (* 1.2 (sin (* 1.0 (al:get-time))))
  ;;                  ;; 0.75
  ;;                  (* (sin (* 1.0 (al:get-time))))
  ;;                  ;; 0.6
  ;;                  (cos (* 1.0 (al:get-time)))
  ;;                  ;; 1.0
  ;;                  ))
  (gl:uniform-matrix-4fv (gl:get-uniform-location light-cube-shader "model")
                         (marr (m*
                                ;; (mrotation +vx+ (degree->radian 15.0))
                                ;; (mrotation +vy+ (degree->radian -10.0))
                                ;; (mtranslation (vec3 1.0 1.0 0.0))
                                (mtranslation light-pos)
                                (mscaling (vec3 0.2 0.2 0.2)))))
  (gl:bind-vertex-array light-cube)
  (gl:draw-arrays :triangles 0 36)
  (gl:bind-vertex-array 0)
  (gl:use-program 0)

  (gl:use-program big-cube-shader)
  (let ((model (m*
                (meye 4)
                (mtranslation (vec3 -0.5 0.0 -1.2))
                (mrotation +vy+ (- (degree->radian (coerce (* 20 (al:get-time)) 'single-float))))
                (mrotation +vx+ (degree->radian -65.0))
                (mrotation +vz+ (degree->radian -45.0))
                (mscaling (vec3 1.7 1.7 1.7))
                )))
    (gl:uniform-matrix-4fv (gl:get-uniform-location big-cube-shader "model")
                           (marr model))
    (gl:uniform-matrix-3fv (gl:get-uniform-location big-cube-shader "normal")
                           (marr (mblock (mtranspose (minv model)) 0 0 3 3))))

  (gl:uniformfv (gl:get-uniform-location big-cube-shader "viewPos") (varr3 camera-position))

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

  (gl:uniformfv (gl:get-uniform-location big-cube-shader "light.position") (varr3 light-pos))
  (gl:uniformfv (gl:get-uniform-location big-cube-shader "light.ambient") (vector 0.2 0.2 0.2))
  (gl:uniformfv (gl:get-uniform-location big-cube-shader "light.diffuse") (vector 1.0 1.0 1.0))
  (gl:uniformfv (gl:get-uniform-location big-cube-shader "light.specular") (vector 0.5 0.5 0.5))

  (gl:uniformf (gl:get-uniform-location big-cube-shader "time") (al:get-time))

  (gl:bind-vertex-array big-cube)
  (gl:draw-arrays :triangles 0 36)
  (gl:bind-vertex-array 0)
  (gl:use-program 0)

  (al:flip-display)
  (sleep 1/60))

(defun init ()
  (al:set-target-backbuffer display)
  (gl:viewport 0 0 800 600)
  (gl:enable :depth-test)

  (ignore-errors
   (gl:delete-program light-cube-shader)
   (gl:delete-program big-cube-shader)
   (gl:delete-vertex-arrays (list light-cube))
   (gl:delete-buffers (list light-cube))
   (gl:delete-vertex-arrays (list big-cube))
   (gl:delete-buffers (list big-cube))
   (gl:delete-texture container-diffuse-map)
   (gl:delete-texture container-specular-map))

  (defparameter light-cube-shader (shader "./vs.vert" "./light-cube.frag"))
  (defparameter big-cube-shader (shader "./vs.vert" "./big-cube.frag"))
  (defparameter container-diffuse-map (texture "container2.png"))
  (defparameter container-specular-map (texture "container2_specular.png"))
  (defparameter container-emission-map (texture "emission.png"))
  (defparameter big-cube (cube))
  (defparameter light-cube (cube))

  (let ((view (mlookat camera-position (v+ camera-position (v- camera-direction)) camera-up))
        (projection (mperspective 45 (/ 800.0 600.0) 0.1 100.0)))
    (gl:use-program big-cube-shader)
    (gl:uniform-matrix-4fv (gl:get-uniform-location big-cube-shader "view") (marr view))
    (gl:uniform-matrix-4fv (gl:get-uniform-location big-cube-shader "projection") (marr projection))
    (gl:use-program light-cube-shader)
    (gl:uniform-matrix-4fv (gl:get-uniform-location light-cube-shader "view") (marr view))
    (gl:uniform-matrix-4fv (gl:get-uniform-location light-cube-shader "projection") (marr projection))
    (gl:use-program 0)))


(defun mainloop ()
  (init)
  (loop
    (with-simple-restart (next-iteration "Continue")
      (render))))

(defvar mainloop-thread (bt:make-thread #'mainloop))

(defun reset ()
  (ignore-errors (bt:destroy-thread mainloop-thread))
  (setf mainloop-thread (bt:make-thread #'mainloop)))
