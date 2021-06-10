(cl:in-package #:sicl-arithmetic)

(defun upgraded-complex-part-type (typespec &optional environment)
  (declare (ignore environment))
  (cond ((subtypep typespec 'double-float) 'double-float)
        ((subtypep typespec 'single-float) 'single-float)
        ((subtypep typespec 'rational) 'rational)
        (t 'nil)))
