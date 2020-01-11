(cl:in-package #:sicl-sequence)

(defmethod length ((datum t))
  (error 'must-be-sequence
         :datum datum))

(defmethod length ((list list))
  (labels ((len (rest counter)
             (if (atom rest)
                 (if (null rest)
                     counter
                     (error 'must-be-proper-list
                            :datum list))
                 (len (cdr rest) (1+ counter)))))
    (len list 0)))

(defmethod length ((vector vector))
  (cl:length vector))
