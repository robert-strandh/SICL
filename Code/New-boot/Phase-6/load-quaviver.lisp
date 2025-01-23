(cl:in-package #:sicl-new-boot-phase-6)

(defun load-quaviver (c4 w4 e4)
  (loop for symbol in '(most-negative-short-float
                        most-positive-short-float
                        least-negative-short-float
                        least-negative-normalized-short-float
                        least-positive-short-float
                        least-positive-normalized-short-float
                        most-negative-single-float
                        most-positive-single-float
                        least-negative-single-float
                        least-negative-normalized-single-float
                        least-positive-single-float
                        least-positive-normalized-single-float
                        most-negative-double-float
                        most-positive-double-float
                        least-negative-double-float
                        least-negative-normalized-double-float
                        least-positive-double-float
                        least-positive-normalized-double-float
                        most-negative-long-float
                        most-positive-long-float
                        least-negative-long-float
                        least-negative-normalized-long-float
                        least-positive-long-float
                        least-positive-normalized-long-float)
        for value = (symbol-value symbol)
        do (clo:make-variable c4 e4 symbol value))
  (setf (clo:fdefinition c4 e4 'coerce) #'coerce)
  (setf (clo:fdefinition c4 e4 'float-digits) #'float-digits)
  (setf (clo:fdefinition c4 e4 'integer-decode-float)
        #'integer-decode-float)
  (setf (clo:fdefinition c4 e4 'byte) #'byte)
  (setf (clo:fdefinition c4 e4 'ldb-test) #'ldb-test)
  (setf (clo:fdefinition c4 e4 'float-radix) #'float-radix)
  #+(or)
  (let ((*features* '(:sicl)))
    (sb:ensure-asdf-system c4 w4 "quaviver")))
