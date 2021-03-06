(defpackage :learnopengl
  (:use :cl :alexandria :3d-matrices :3d-vectors))

(in-package :learnopengl)

;;;;
;;;; Glue utils
;;;;

(defun init-display (&key (x 800) (y 600))
  (al:init)
  (al:set-new-display-flags '(:windowed :opengl))
  (al:set-new-display-option :vsync 0 :require)

  ;; OpenGL does not guarantee a depth buffer by default.
  ;;
  ;; This is required to prevent drawing over textures that are behind
  ;; others (i.e. to make glClear(GL_DEPTH_BUFFER_BIT) work)
  (al:set-new-display-option :depth-size 16 :suggest)

  ;; Enable MSAA
  (al:set-new-display-option :sample-buffers 1 :suggest)
  (al:set-new-display-option :samples 4 :suggest)

  (al:create-display x y))

(defun destroy-display (display)
  (al:destroy-display display)
  (al:uninstall-system))

(defmacro with-display (&body body)
  `(progn
     #+swank (al:set-target-backbuffer *display*)
     ,@body))

(defun gl-array (type elems)
  (check-type elems vector)
  (let ((arr (gl:alloc-gl-array type (length elems))))
    (dotimes (i (length elems))
      (setf (gl:glaref arr i) (aref elems i)))
    arr))

(declaim (inline degree->radian radian->degree)
         (ftype (function (single-float) single-float) degree->radian radian->degree))

(defun degree->radian (degree)
  (declare (optimize speed))
  (coerce (* degree (/ pi 180)) 'single-float))

(defun radian->degree (radian)
  (declare (optimize speed))
  (coerce (* radian (/ 180 pi)) 'single-float))

(defmacro -> (&body forms)
  (reduce (lambda (expansion form)
            (if (consp form)
                (append (list (first form))
                        (list expansion)
                        (rest form))
                (append (list form) (list expansion))))
          forms))

(defmacro ->> (&body forms)
  (reduce (lambda (expansion form)
            (if (consp form)
                (append form (list expansion))
                (append (list form) (list expansion))))
          forms))


(defvar display nil)
(defvar event-queue nil)

(defun create-context (&optional (x 800) (y 600))
  (setf display (init-display :x x :y y))
  (setf event-queue (al:create-event-queue))
  (al:install-keyboard)
  (al:install-mouse)
  (al:register-event-source event-queue (al:get-mouse-event-source))
  (al:register-event-source event-queue (al:get-keyboard-event-source))
  (al:register-event-source event-queue (al:get-display-event-source display))
  (gl:enable :depth-test)
  (gl:enable :multisample)
  (gl:viewport 0 0 x y))

(defun destroy-context ()
  (al:destroy-event-queue event-queue)
  (al:destroy-display display)
  (setf event-queue (cffi:null-pointer))
  (setf display (cffi:null-pointer)))


(defun varr3 (v)
  (declare (optimize speed)
           (type vec3 v))
  (vector (vx3 v) (vy3 v) (vz3 v)))

(defmacro dovec ((var vec) &rest body)
  `(loop for ,var across ,vec
         do ,@body))


(defun mapv (fn vec)
  (map 'vector fn vec))

(defun info (format-string &rest args)
  (apply #'format `(t ,(uiop:strcat format-string "~%") ,@args)))


(defun vector* (&rest elems)
  (make-array (length elems) :adjustable t :fill-pointer 0 :initial-contents elems))
