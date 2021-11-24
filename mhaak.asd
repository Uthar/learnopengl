

(defsystem :mhaak
  :depends-on (:cl-liballegro :cl-opengl :3d-matrices :3d-vectors)
  :components ((:file "common")
               (:file "eye/eye" :depends-on ("common"))))
