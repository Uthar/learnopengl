(require :asdf)
(asdf:load-system :cl-liballegro)
(asdf:load-system :cl-opengl)

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


;;;;
;;;; Proper code
;;;;

(defvar *display*)

(defparameter vertices (gl-array :float (vector 0.0  0.2 -1.0
                                                0.2 -0.5 0.0
                                                -0.7 -0.5 1.0)))
(defparameter vbo nil)
(defparameter vao nil)

;; My GPU supports only OpenGL 3.0 and GLSL 1.3

(defparameter vs-source "
  #version 130
  in vec3 aPos;

  void main()
  {
      gl_Position = vec4(aPos.x, aPos.y, aPos.z, 1.0);
  }
")

(defparameter fs-source "
  #version 130
  out vec4 FragColor;

  void main()
  {
      FragColor = vec4(1.0f, 0.5f, 0.2f, 1.0f);
  }
")

(defparameter vs nil)
(defparameter fs nil)
(defparameter shader-program nil)

(defun render ()

  ;; Draw a background
  (gl:clear-color 0.6 0.2 0.2 1.0)
  (gl:clear :color-buffer-bit)

  ;; Draw the triangle from the provided vertices after passing
  ;; them through the shaders as configured in `main`
  (gl:use-program shader-program)
  (gl:bind-vertex-array vao)
  (gl:draw-arrays :triangles 0 3)

  (al:flip-display))

(defun mainloop ()
  (loop
    (funcall 'render)
    (sleep 1/30)))

(defun main ()
  (unwind-protect
       (progn

         ;;
         ;; Following https://learnopengl.com/Getting-started/Hello-Triangle
         ;;

         ;; Open an OpenGL window
         (setf *display* (init-display))

         ;; Create the VBO and VAO
         ;;
         ;; The VBO stores my vertex data in GPU memory
         ;; The VAO remembers VBO's configuration, e.g. vertex attribute pointers
         (setf vbo (gl:gen-buffer))
         (setf vao (gl:gen-vertex-array))

         ;; Remember VBO configuration when the VAO will be bound in the future
         (gl:bind-vertex-array vao)

         ;; Copy the vertex data to GPU memory
         (gl:bind-buffer :array-buffer vbo)
         (gl:buffer-data :array-buffer :static-draw vertices)

         ;; Setup the vertex shader
         ;;
         ;; The vertex shader receives the vertex data as input and
         ;; returns a vec4 in gl_Position. It operates on one vertex
         ;; at a time.
         (setf vs (gl:create-shader :vertex-shader))
         (gl:shader-source vs vs-source)
         (gl:compile-shader vs)
         (format t (gl:get-shader-info-log vs))

         ;; Setup the fragment shader
         ;;
         ;; The fragment shader receives fragments and returns a
         ;; colour. It operates on one fragment at a time.
         (setf fs (gl:create-shader :fragment-shader))
         (gl:shader-source fs fs-source)
         (gl:compile-shader fs)
         (format t (gl:get-shader-info-log fs))

         ;; Setup the shader program
         ;;
         ;; The shader program combines multiple shaders to run on the
         ;; GPU as part of the graphics pipeline.
         (setf shader-program (gl:create-program))
         (gl:attach-shader shader-program vs)
         (gl:attach-shader shader-program fs)
         (gl:link-program shader-program)
         (format t (gl:get-program-info-log shader-program))

         ;; Link vertex attributes
         ;;
         ;; This teaches OpenGL how to pass the input vertex data as
         ;; arguments to the vertex shader.
         (gl:vertex-attrib-pointer 0 3 :float nil 0 (cffi:null-pointer))
         (gl:enable-vertex-attrib-array 0)

         (mainloop))
    (destroy-display *display*)))

;; (setf game-thread (bt:make-thread 'main :name "Game thread"))
;; (bt:destroy-thread game-thread)
