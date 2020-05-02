(cl:in-package #:sicl-sequence)

(defmethod reverse ((list list))
  (let ((result '()))
    (dolist (elt list result)
      (push elt result))))

(seal-domain #'reverse '(list))

(replicate-for-each-relevant-vectoroid #1=#:vectoroid
  (defmethod reverse ((vectoroid #1#))
    (let* ((n (length vectoroid))
           (result (make-array n :element-type (array-element-type vectoroid))))
      (loop for index below n do
        (setf (elt result index)
              (elt vectoroid (- n index 1))))
      result)))

(seal-domain #'reverse '(vector))
