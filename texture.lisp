

(in-package :learnopengl)

(defun texture (path)
  (multiple-value-bind (bytes width height format) (read-image path)
    (let ((texture (gl:gen-texture)))
      (gl:bind-texture :texture-2d texture)
      (gl:tex-image-2d :texture-2d 0 format width height 0 format :unsigned-byte bytes)
      (gl:generate-mipmap :texture-2d)
      (gl:bind-texture :texture-2d 0)
      texture)))

(defun read-image (path)
  (switch ((pathname-type path) :test #'string=)
    ("jpg" (read-image-jpeg path))
    ("png" (read-image-png path))
    (t (error "Format ~a not supported" (pathname-type path)))))

(defun read-image-png (path)
  (let*
      ((image (png-read:read-png-file path))
       (width (png-read:width image))
       (height (png-read:height image))
       (format (map-format (png-read:colour-type image)))
       (image-data (png-read:image-data image))
       (bytes
         (make-array (array-total-size image-data)
                     :displaced-to image-data
                     :element-type (array-element-type image-data))))
    (values bytes width height format)))

(defun read-image-jpeg (path)
  (multiple-value-bind (bytes height width _ format) (jpeg:decode-image path)
    (declare (ignore _))
    (values bytes width height (map-format format))))

(defun map-format (format)
  (case format
    (:truecolor :rgb)
    (:truecolor-alpha :rgba)
    (:ycbcr-rgb :rgb)
    (t (error "Format ~a not supported" format))))
