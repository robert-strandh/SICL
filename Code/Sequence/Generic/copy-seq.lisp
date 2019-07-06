(cl:in-package #:sicl-sequence)

(defmethod copy-seq ((datum t))
  (error 'must-be-sequence
         :datum datum))

(defmethod copy-seq ((list list))
  (subseq list 0))

(replicate-for-each-relevant-vectoroid #1=#:vectoroid
  (defmethod copy-seq ((vectoroid #1#))
    (subseq vectoroid 0)))
