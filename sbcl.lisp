(in-package :learnopengl)

(defun print-gc-time ()
  (format t "GC took ~a seconds~%" (/ sb-ext:*gc-run-time*
                                      internal-time-units-per-second
                                      1.0))
  (setf sb-ext:*gc-run-time* 0))

(pushnew #'print-gc-time sb-ext:*after-gc-hooks*)
