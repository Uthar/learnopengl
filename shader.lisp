;; Shader compilation.

(in-package :learnopengl)

(defun link-shader-program (vs fs)
  (let ((id (gl:create-program)))
    (assert (plusp id) (id) "Cannot create GL shader program")
    (gl:attach-shader id vs)
    (gl:attach-shader id fs)
    (gl:link-program id)
    (let ((log (gl:get-program-info-log id)))
      (unless (gl:get-program id :link-status)
        (error "Error linking shader program: ~a" log))
      (unless (emptyp log)
        (warn "During compiling of shader program: ~a" log)))
    id))

(defun compile-shader (type path)
  (let ((src (read-file-into-string path))
        (id (gl:create-shader type)))
    (assert (plusp id) (id) "Cannot create GL shader")
    (gl:shader-source id src)
    (gl:compile-shader id)
    (let ((log (gl:get-shader-info-log id)))
      (unless (gl:get-shader id :compile-status)
        (error "Error compiling shader: ~a" log))
      (unless (emptyp log)
        (warn "During compiling of shader ~a: ~a" path log)))
    id))

(defun shader (vs-path fs-path)
  (let ((vs nil)
        (fs nil)
        (id nil))
    (unwind-protect
         (setf vs (compile-shader :vertex-shader vs-path)
               fs (compile-shader :fragment-shader fs-path)
               id (link-shader-program vs fs))
      (ignore-errors
       (gl:delete-shader vs)
       (gl:delete-shader fs)))))
