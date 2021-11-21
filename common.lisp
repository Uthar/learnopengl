(defpackage :learnopengl
  (:use :cl))

(in-package :learnopengl)

;;;;
;;;; Glue utils
;;;;

(defun init-display (&key (x 800) (y 600))
  (al:init)
  (al:init-primitives-addon)
  (al:set-new-display-flags '(:windowed :resizable :opengl))
  (al:set-new-display-option :vsync 0 :require)
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

