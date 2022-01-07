

(in-package :learnopengl)


(defun texture (path)
  (let*
      ((png-image
         (png-read:read-png-file path))

       (image-data (png-read:image-data png-image))
       (width (png-read:width png-image))
       (height (png-read:height png-image))

       (bytes
         (make-array (array-total-size image-data)
                     :displaced-to image-data
                     :element-type (array-element-type image-data)))

       (texture (gl:gen-texture)))

    (gl:bind-texture :texture-2d texture)
    (gl:tex-image-2d :texture-2d 0 :rgba width height 0 :rgba :unsigned-byte bytes)
    (gl:generate-mipmap :texture-2d)
    (gl:bind-texture :texture-2d 0)
    texture))
