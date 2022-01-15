
#+(and sbcl 64-bit)
(in-package :sb-c)

#+(and sbcl 64-bit)
(eval-when (:compile-toplevel)
  (defknown (learnopengl::%rsqrt)
      (single-float) (single-float 0.0)
      (foldable flushable movable)))

#+(and sbcl 64-bit)
(in-package :sb-vm)

#+(and sbcl 64-bit)
(eval-when (:compile-toplevel)
  (define-vop (learnopengl::%rsqrt)
    (:args (x :scs (single-reg)))
    (:results (y :scs (single-reg)))
    (:translate learnopengl::%rsqrt)
    (:policy :fast-safe)
    (:arg-types single-float)
    (:result-types single-float)
    (:save-p :compute-only)
    (:generator 1
                (inst rsqrtss y x))))

(in-package :learnopengl)

(declaim (ftype (function (single-float) single-float) rsqrt))

#+(and sbcl 64-bit)
(defun rsqrt (x)
  (declare (optimize speed))
  (%rsqrt x))

#-(and sbcl 64-bit)
(defun rsqrt (x)
    (declare (optimize speed))
    (/ 1 (sqrt x)))
