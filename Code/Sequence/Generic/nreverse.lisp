(cl:in-package #:sicl-sequence)

(defmethod nreverse ((list list) &aux (result '()))
  (loop until (endp list) do
    (rotatef list (cdr list) result))
  result)

(seal-domain #'nreverse '(list))

(replicate-for-each-relevant-vectoroid #1=#:vectoroid
  (defmethod nreverse ((vectoroid #1#))
    (let ((n (length vectoroid)))
      (loop for index below (floor n 2) do
        (rotatef (aref vectoroid index)
                 (aref vectoroid (- n index 1)))))
    vectoroid))

(seal-domain #'nreverse '(vector))
