(cl:in-package #:sicl-new-boot-phase-6)

(defun load-quaviver (c4 w4 e4)
  (declare (ignorable w4))
  (let ((single-float
          (make-instance 'sb:simulated-single-float
            :bit-pattern buoy-simulate:most-positive-single-float)))
    (clo:make-variable c4 e4 'most-positive-short-float single-float)
    (clo:make-variable c4 e4 'most-positive-single-float single-float))
  (setf (clo:fdefinition c4 e4 'coerce) #'coerce)
  (setf (clo:fdefinition c4 e4 'byte) #'byte)
  (setf (clo:fdefinition c4 e4 'ldb-test) #'ldb-test)
;;  #+(or)
  (let ((*features* '(:sicl)))
    (sb:ensure-asdf-system c4 w4 "quaviver")))
