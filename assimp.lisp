(in-package :learnopengl)

(defstruct vertex
  position
  normal
  texture-coords)

(defstruct texture
  id
  type)

(defclass mesh ()
  ((vertices :initarg :vertices :initform (vector*))
   (indices  :initarg :indices  :initform (vector*))
   (textures :initarg :textures :initform (vector*))
   (vao :initform (gl:create-vertex-array))
   (vbo :initform (gl:create-buffer))
   (ebo :initform (gl:create-buffer))))

(defmethod initialize-instance :after ((mesh mesh) &key)
  (info "initialize instance of mesh")
  (with-slots (vertices indices textures vao vbo ebo) mesh
    (gl:bind-vertex-array vao)

    (gl:bind-buffer :array-buffer vbo)
    (gl:buffer-data :array-buffer :static-draw (gl-array :float vertices))
    (gl:vertex-attrib-pointer 0 3 :float nil
                              (* 9 (cffi:foreign-type-size :float)) 0)
    (gl:vertex-attrib-pointer 1 3 :float nil
                              (* 9 (cffi:foreign-type-size :float))
                              (* 3 (cffi:foreign-type-size :float)))
    (gl:vertex-attrib-pointer 2 3 :float nil
                              (* 9 (cffi:foreign-type-size :float))
                              (* 6 (cffi:foreign-type-size :float)))
    (gl:enable-vertex-attrib-array 0)
    (gl:enable-vertex-attrib-array 1)
    (gl:enable-vertex-attrib-array 2)

    (gl:bind-buffer :element-array-buffer ebo)
    (gl:buffer-data :element-array-buffer :static-draw (gl-array :unsigned-int indices))

    (gl:bind-vertex-array 0)
    (gl:bind-buffer :array-buffer 0)
    (gl:bind-buffer :element-array-buffer 0)))

(defclass model ()
  ((meshes :initform (vector*))
   (directory :initform nil)
   (textures :initform (make-hash-table :test 'equal)
             :allocation :class))
  (:default-initargs
   :path (error "path required")))

(defmethod initialize-instance :after ((model model) &key path)
  (info "initialize instance of model")
  (assert (probe-file path) (path))
  (with-slots (directory) model
    (setf directory (pathname-directory path)))
  (load-model model path))

(defmethod load-model ((model model) path)
  (info "call load-model")
  (let ((scene (ai:import-into-lisp
                path
                :processing-flags '(:ai-process-triangulate :ai-process-flip-u-vs))))
    (process-node model (ai:root-node scene) scene)))

;; (make-instance 'model :path "/dev/null")

(defun process-node (model node scene)
  (info "process-node ~A" (ai:name node))
  (loop for i across (ai:meshes node)
        do (vector-push-extend
            (process-mesh model (svref (ai:meshes scene) i) scene)
            (slot-value model 'meshes)))
  (loop for child across (ai:children node)
        do (process-node model child scene)))

(defun process-mesh (model ai-mesh scene)
  (info "process-mesh" )
  (let ((vertices (vector*))
        (indices (vector*))
        (textures (vector*)))
    (loop for pos
          across (make-array (array-total-size (ai:vertices ai-mesh))
                             :element-type (array-element-type (ai:vertices ai-mesh))
                             :displaced-to (ai:vertices ai-mesh))
          for norm
          across (make-array (array-total-size (ai:normals ai-mesh))
                             :element-type (array-element-type (ai:normals ai-mesh))
                             :displaced-to (ai:normals ai-mesh))
          for texcoord
          across (make-array (array-total-size (first-elt (ai:texture-coords ai-mesh)))
                             :element-type (array-element-type (first-elt (ai:texture-coords ai-mesh)))
                             :displaced-to (first-elt (ai:texture-coords ai-mesh)))
          do (vector-push-extend (aref pos 0) vertices)
             (vector-push-extend (aref pos 1) vertices)
             (vector-push-extend (aref pos 2) vertices)
             (vector-push-extend (aref norm 0) vertices)
             (vector-push-extend (aref norm 1) vertices)
             (vector-push-extend (aref norm 2) vertices)
             (vector-push-extend (aref texcoord 0) vertices)
             (vector-push-extend (aref texcoord 1) vertices)
             (vector-push-extend (aref texcoord 2) vertices))
    (loop for i
          across (make-array (array-total-size (ai:faces ai-mesh))
                             :element-type (array-element-type (ai:faces ai-mesh))
                             :displaced-to (ai:faces ai-mesh))
          do (vector-push-extend (aref i 0) indices)
             (vector-push-extend (aref i 1) indices)
             (vector-push-extend (aref i 2) indices))
    (let ((material-index (ai:material-index ai-mesh)))
      (when (>= material-index 0)
        (let ((material (svref (ai:materials scene) material-index)))
          (dovec (tex (load-material-textures model material :ai-texture-type-diffuse "texture_diffuse"))
            (vector-push-extend tex textures))
          (dovec (tex (load-material-textures model material :ai-texture-type-specular "texture_specular"))
            (vector-push-extend tex textures))
          (dovec (tex (load-material-textures model material :ai-texture-type-height "texture_normal"))
            (vector-push-extend tex textures))
          (dovec (tex (load-material-textures model material :ai-texture-type-ambient "texture_height"))
            (vector-push-extend tex textures)))))
    (make-instance 'mesh :vertices vertices :indices indices :textures textures)))


(defun load-material-textures (model material type name)
  (info "load-material-textures ~A" name)
  (let* ((textures (vector*))
         (tex-files (remove-if-not
                     (lambda (tex) (eq (car tex) type))
                     (gethash "$tex.file" material))))
    (dolist (tex tex-files)
      (destructuring-bind (ai-type id file) tex
        (declare (ignorable ai-type id))
        (vector-push-extend
         (make-texture :id (texture-from-file model file)
                       :type name)
         textures)))
    textures))

(defun texture-from-file (model file)
  (info "texture-from-file ~A" file)     ;; FIXME store as pathname directly
  (let ((pathname (merge-pathnames file (make-pathname :directory (slot-value model 'directory)))))
    (or
     (gethash pathname (slot-value model 'textures))
     (let ((id (gl:gen-texture)))
       (multiple-value-bind (bytes width height format) (read-image pathname)
         (gl:bind-texture :texture-2d id)
         (gl:tex-image-2d :texture-2d 0 (format-bytes-per-pixel format) width height 0 format :unsigned-byte bytes)
         (gl:generate-mipmap :texture-2d)
         (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
         (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
         (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
         (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
         (setf (gethash pathname (slot-value model 'textures)) id))))))


(defmethod draw ((model model) shader)
    (dovec (mesh (slot-value model 'meshes))
      (draw mesh shader)))

(defmethod draw ((mesh mesh) shader)
  (declare (optimize speed))
  (let ((diffuse 1)
        (specular 1)
        (normal 1)
        (height 1)
        (id 1))
    (declare (type fixnum diffuse specular normal height id))
    (with-slots (vao indices textures) mesh
      (gl:bind-vertex-array vao)

      (let ((ids (map 'vector #'texture-id textures))
            (id -1)
            (i -1))

        (gl:active-texture (incf i))
        (gl:bind-texture :texture-2d (aref ids (incf id)))
        (gl:uniformi (gl:get-uniform-location shader "texture_diffuse1") i)
        ;; (gl:active-texture (incf i))
        ;; (gl:bind-texture :texture-2d (aref ids (incf i)))
        ;; (gl:uniformi (gl:get-uniform-location shader "texture_diffuse2") 0)

        (gl:active-texture (incf i))
        (gl:bind-texture :texture-2d (aref ids (incf id)))
        (gl:uniformi (gl:get-uniform-location shader "texture_specular1") i))
        ;; (gl:active-texture (incf i))
        ;; (gl:bind-texture :texture-2d (aref ids (incf i)))
        ;; (gl:uniformi (gl:get-uniform-location shader "texture_specular2") 0)

        ;; (gl:active-texture (incf i))
        ;; (gl:bind-texture :texture-2d (aref ids (incf i)))
        ;; (gl:uniformi (gl:get-uniform-location shader "texture_normal1") 0)
        ;; (gl:active-texture (incf i))
        ;; (gl:bind-texture :texture-2d (aref ids (incf i)))
        ;; (gl:uniformi (gl:get-uniform-location shader "texture_normal2") 0)

        ;; (gl:active-texture (incf i))
        ;; (gl:bind-texture :texture-2d (aref ids (incf i)))
        ;; (gl:uniformi (gl:get-uniform-location shader "texture_height1") 0)
        ;; (gl:active-texture (incf i))
        ;; (gl:bind-texture :texture-2d (aref ids (incf i)))
        ;; (gl:uniformi (gl:get-uniform-location shader "texture_height2") 0)

      ;; (loop
      ;;   for texture across textures
      ;;   for i fixnum from 0
      ;;   for type of-type simple-string = (texture-type texture)
      ;;   do
      ;;      (gl:active-texture i)
      ;;      (switch (type :test #'string=)
      ;;        ("texture_diffuse"  (setf id (incf diffuse)))
      ;;        ("texture_specular" (setf id (incf specular)))
      ;;        ("texture_normal"   (setf id (incf normal)))
      ;;        ("texture_height"   (setf id (incf height))))

      ;;      ;; FIXME Seriously this is taking 50% of the cpu?
      ;;      (gl:uniformi (gl:get-uniform-location shader
      ;;                                            ;; (format nil "~a~a" (texture-type texture) id))
      ;;                                            (concatenate 'string type (vector (code-char (+ 48 i)))))
      ;;                   i)

      ;;      (gl:bind-texture :texture-2d (texture-id texture)))

      (gl:draw-elements :triangles
                        (gl:make-null-gl-array :unsigned-int)
                        :count (length indices))
      (gl:bind-vertex-array 0)
      (gl:active-texture 0))))
