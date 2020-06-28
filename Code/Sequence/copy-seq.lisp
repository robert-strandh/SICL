(cl:in-package #:sicl-sequence)

(defmethod copy-seq ((list list))
  (loop for element in list collect element))

(seal-domain #'copy-seq '(list))

(replicate-for-each-vector-class #1=#:vector-class
  (defmethod copy-seq ((vector #1#))
    (make-sequence-like vector (length vector) :initial-contents vector)))

(seal-domain #'copy-seq '(vector))
