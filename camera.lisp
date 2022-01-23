;; Camera system for "walking" around a scene
;;
;; Not thread safe.

(in-package :learnopengl)

(defclass camera ()
  ((position  :initform (vec3 0 0 3) :initarg :position)
   (direction :initform (vec3 0 0 -1))
   (zoom  :initform 45.0)
   (yaw   :initform -90.0)
   (pitch :initform 0.0)))

(defmethod initialize-instance :after ((camera camera) &key)
  (with-slots (direction yaw pitch) camera
    (setf direction (direction-vector yaw pitch))))

(defun direction-vector (yaw pitch)
  (let ((x (* (cos (degree->radian yaw)) (cos (degree->radian pitch))))
        (y (sin (degree->radian pitch)))
        (z (* (sin (degree->radian yaw)) (cos (degree->radian pitch)))))
    (v- (vunit (vec3 x y z)))))

(defun camera (x y z)
  (make-instance 'camera :position (vec3 x y z)))

(defmethod view-matrix ((camera camera))
  (with-slots (position direction) camera
    (mlookat position (v- position direction) +vy+)))

(defmethod look-around ((camera camera) x-offset y-offset)
  (check-type x-offset single-float)
  (check-type y-offset single-float)
  (with-slots (yaw pitch direction) camera
    (setf yaw (+ yaw x-offset)
          pitch (clamp (- pitch y-offset) -89.0 89.0)
          direction (direction-vector yaw pitch))
    (view-matrix camera)))

(defmethod walk ((camera camera) &key (x 0.0) (y 0.0) (z 0.0))
  (check-type x single-float)
  (check-type y single-float)
  (check-type z single-float)
  (with-slots (position direction) camera
    (nv+ position (v* (vunit (vc direction +vy+)) x))
    (nv+ position (v* (v- (vc direction (vunit (vc direction +vy+)))) y))
    (nv+ position (v* direction z)))
  (view-matrix camera))
