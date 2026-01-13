(cl:in-package #:sicl-new-boot-phase-6)

(eval-when (:compile-toplevel) (sb:enable-parcl-symbols c4))

(defun bits-to-single-float (c4 e4 bits)
  (make-instance 'sb:header
    :class (clo:find-class c4 e4 'single-float)
    :rack (make-array 1 :initial-element bits)))

(defun bits-to-double-float (c4 e4 bits)
  (make-instance 'sb:header
    :class (clo:find-class c4 e4 'double-float)
    :rack (make-array 1 :initial-element bits)))

(defun load-quaviver (c4 w4 e4)
  (declare (ignorable w4))
  (setf (clo:fdefinition c4 e4 'float-radix)
        (lambda (float)
          (declare (ignore float))
          2))
  ;; (sb:ensure-asdf-system c4 w4 "sicl-type-coerce")
  (sb:ensure-asdf-system c4 w4 "sicl-arithmetic-floating-point")
  (setf (clo:fdefinition c4 e4 'byte) #'byte)
  (setf (clo:fdefinition c4 e4 'ldb-test) #'ldb-test)
  (setf (clo:fdefinition c4 e4 'coerce)
        (lambda (value type)
          (assert (zerop value))
          (ecase type
            (single-float (bits-to-single-float c4 e4 0))
            (double-float (bits-to-double-float c4 e4 0)))))
  (let ((*features* '(:sicl)))
    (sb:ensure-asdf-system c4 w4 "quaviver")))
