(cl:in-package #:sicl-sequence)

(defmethod delete-if-not (test (list list) &key from-end (start 0) end count key)
  (with-predicate (predicate test)
    (with-key-function (key key)
      (delete-in-list
       (lambda (x) (not (predicate (key x))))
       list from-end start end count))))

(replicate-for-each-relevant-vectoroid #1=#:vectoroid
  (defmethod delete-if-not (test (vectoroid #1#) &key from-end (start 0) end count key)
    (with-predicate (predicate test)
      (with-key-function (key key)
        (delete-in-vector
         (lambda (x) (not (predicate (key x))))
         vectoroid from-end start end count)))))
