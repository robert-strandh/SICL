(cl:in-package #:sicl-sequence)

(defmethod reverse ((list list))
  (let ((result '()))
    (dolist (elt list result)
      (push elt result))))

(seal-domain #'reverse '(list))

(replicate-for-each-vector-class #1=#:vector-class
  (defmethod reverse ((vector #1#))
    (let* ((n (length vector))
           (result (make-array n :element-type (array-element-type vector))))
      (loop for index below n do
        (setf (elt result index)
              (elt vector (- n index 1))))
      result)))

(seal-domain #'reverse '(vector))
