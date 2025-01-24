(cl:in-package #:sicl-new-boot-phase-6)

(eval-when (:compile-toplevel) (sb:enable-parcl-symbols c4))

(defun load-quaviver (c4 w4 e4)
  (declare (ignorable w4))
  (let ((single-float
          (make-instance 'sb:simulated-single-float
            :bit-pattern buoy-simulate:most-positive-single-float)))
    (clo:make-variable c4 e4 'most-positive-short-float single-float)
    (clo:make-variable c4 e4 'most-positive-single-float single-float))
  (let ((single-float
          (make-instance 'sb:simulated-single-float
            :bit-pattern buoy-simulate:least-positive-single-float)))
    (clo:make-variable c4 e4 'least-positive-short-float single-float)
    (clo:make-variable c4 e4 'least-positive-single-float single-float))
  (let ((single-float
          (make-instance 'sb:simulated-single-float
            :bit-pattern
            buoy-simulate:least-positive-normalized-single-float)))
    (clo:make-variable
     c4 e4 'least-positive-normalized-short-float single-float)
    (clo:make-variable
     c4 e4 'least-positive-normalized-single-float single-float))
  (let ((single-float
          (make-instance 'sb:simulated-single-float
            :bit-pattern buoy-simulate:most-negative-single-float)))
    (clo:make-variable c4 e4 'most-negative-short-float single-float)
    (clo:make-variable c4 e4 'most-negative-single-float single-float))
  (let ((make-instance (clo:fdefinition c4 e4 'make-instance))
        (setf-standard-instance-access
          (let ((symbol @clostrophilia:standard-instance-access))
            (clo:fdefinition c4 e4 `(setf ,symbol)))))
    (let ((double-float
            (funcall make-instance 'double-float :additional-size 1)))
      (funcall setf-standard-instance-access
               buoy-simulate:most-positive-double-float
               double-float 0)
      (clo:make-variable c4 e4 'most-positive-long-float double-float)
      (clo:make-variable c4 e4 'most-positive-double-float double-float))
    (let ((double-float
            (funcall make-instance 'double-float :additional-size 1)))
      (funcall setf-standard-instance-access
               buoy-simulate:most-negative-double-float
               double-float 0)
      (clo:make-variable c4 e4 'most-negative-long-float double-float)
      (clo:make-variable c4 e4 'most-negative-double-float double-float))
    (let ((double-float
            (funcall make-instance 'double-float :additional-size 1)))
      (funcall setf-standard-instance-access
               buoy-simulate:least-positive-double-float
               double-float 0)
      (clo:make-variable c4 e4 'least-positive-long-float double-float)
      (clo:make-variable c4 e4 'least-positive-double-float double-float)))
  (setf (clo:fdefinition c4 e4 'float-digits)
        (lambda (float)
          (if (typep float 'sb:simulated-single-float) 24 53)))
  (setf (clo:fdefinition c4 e4 'integer-decode-float)
        (lambda (float)
          (if (typep float 'sb:simulated-single-float)
              (buoy-simulate:integer-decode-single-float
               (sb:bit-pattern float))
              (let ((symbol @clostrophilia:standard-instance-access))
                (buoy-simulate:integer-decode-double-float
                 (funcall (clo:fdefinition c4 e4 symbol) float 0))))))
  (setf (clo:fdefinition c4 e4 'float-radix)
        (lambda (float)
          (declare (ignore float))
          2))
  (setf (clo:fdefinition c4 e4 'coerce) #'coerce)
  (setf (clo:fdefinition c4 e4 'byte) #'byte)
  (setf (clo:fdefinition c4 e4 'ldb-test) #'ldb-test)
;;  #+(or)
  (let ((*features* '(:sicl)))
    (sb:ensure-asdf-system c4 w4 "quaviver")))
