(cl:in-package #:sicl-sequence)

(defmethod remove-duplicates ((list list) &key from-end test test-not (start 0) end key)
  (delete-duplicates
   (copy-seq list)
   :from-end from-end :test test :test-not test-not :start start :end end :key key))

(seal-domain #'remove-duplicates '(list))

(replicate-for-each-vector-class #1=#:vector-class
  (defmethod remove-duplicates ((vector #1#) &key from-end test test-not (start 0) end key)
    (delete-duplicates
     (copy-seq vector)
     :from-end from-end :test test :test-not test-not :start start :end end :key key)))

(seal-domain #'remove-duplicates '(vector))
