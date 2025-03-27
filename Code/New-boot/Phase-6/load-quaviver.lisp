(cl:in-package #:sicl-new-boot-phase-6)

(eval-when (:compile-toplevel) (sb:enable-parcl-symbols c4))

(defun load-quaviver (c4 w4 e4)
  (declare (ignorable w4))
  (setf (clo:fdefinition c4 e4 'float-radix)
        (lambda (float)
          (declare (ignore float))
          2))
  (sb:ensure-asdf-system c4 w4 "sicl-type-coerce")
  (sb:ensure-asdf-system c4 w4 "sicl-arithmetic-floating-point")
  (setf (clo:fdefinition c4 e4 'byte) #'byte)
  (setf (clo:fdefinition c4 e4 'ldb-test) #'ldb-test)
  ;; This is not quite correct because it assumes that the host floats
  ;; are the same as the target floats.
  (setf (clo:fdefinition c4 e4 'float) #'float)
  (let ((*features* '(:sicl)))
    (sb:ensure-asdf-system c4 w4 "quaviver")))
