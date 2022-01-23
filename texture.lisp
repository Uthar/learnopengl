;; Texture loading.

(in-package :learnopengl)

(defun texture (path)
  (multiple-value-bind (bytes width height format) (read-image path)
    (let ((texture (gl:gen-texture)))
      (gl:bind-texture :texture-2d texture)
      (gl:tex-image-2d :texture-2d 0 (format-bytes-per-pixel format) width height 0 format :unsigned-byte bytes)
      (gl:generate-mipmap :texture-2d)
      (gl:bind-texture :texture-2d 0)
      texture)))

;; FIXME: Unused
(defun flip-image-vertically (bytes width height format)
  (values
   (flip-vector-vertically bytes height (* width (format-bytes-per-pixel format)))
   width
   height
   format))

(defun flip-vector-vertically (vector height width)
  (loop with flipped = (make-array (array-total-size vector)
                                   :element-type (array-element-type vector))
        for row1 from 0 upto (1- height)
        for row2 from (1- height) downto 0
        do (loop for pixel from 0 upto (1- width)
                 do (setf (aref flipped (+ (* row1 width) pixel))
                          (aref vector (+ (* row2 width) pixel))))
        finally (return flipped)))

(defun read-image (path &key (flip t))
  (multiple-value-bind (bytes width height format)
      (switch ((pathname-type path) :test #'string=)
        ("jpg" (read-image-jpeg path))
        ("png" (read-image-png path))
        (t (error "Format ~a not supported" (pathname-type path))))
    (values
     (if flip
         (flip-vector-vertically bytes height (* (format-bytes-per-pixel format) width))
         bytes)
     width height format)))

(defun format-bytes-per-pixel (format)
  (case format
    ((:rgb :bgr) 3)
    (:rgba 4)
    (t (error "Format ~a not supported" format))))

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
    (:indexed-colour :rgb)
    ;; FIXME not sure
    (:truecolor-alpha :rgba)
    (:ycbcr-rgb :bgr)
    (t (error "Format ~a not supported" format))))
