(defpackage :learnopengl
  (:use :cl :alexandria :3d-matrices :3d-vectors))

(in-package :learnopengl)

;;;;
;;;; Glue utils
;;;;

(defun init-display (&key (x 800) (y 600))
  (al:init)
  (al:init-primitives-addon)
  (al:set-new-display-flags '(:windowed :resizable :opengl))
  (al:set-new-display-option :vsync 0 :require)

  ;; OpenGL does not guarantee a depth buffer by default.
  ;;
  ;; This is required to prevent drawing over textures that are behind
  ;; others (i.e. to make glClear(GL_DEPTH_BUFFER_BIT) work)
  (al:set-new-display-option :depth-size 16 :suggest)

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
