(cl:in-package #:sicl-sequence)

(defmethod nreverse ((list list) &aux (result '()))
  (loop until (endp list) do
    (rotatef list (cdr list) result))
  result)

(seal-domain #'nreverse '(list))

(replicate-for-each-vector-class #1=#:vector-class
  (defmethod nreverse ((vector #1#))
    (let ((n (length vector)))
      (loop for index below (floor n 2) do
        (rotatef (elt vector index)
                 (elt vector (- n index 1)))))
    vector))

(seal-domain #'nreverse '(vector))
