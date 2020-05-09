(cl:in-package #:sicl-sequence)

(defmethod delete-if (test (list list) &key from-end (start 0) end count key)
  (with-predicate (predicate test)
    (with-key-function (key key)
      (delete-in-list
       (lambda (x) (predicate (key x)))
       list from-end start end count))))

(seal-domain #'delete-if '(t list))

(replicate-for-each-vector-class #1=#:vector-class
  (defmethod delete-if (test (vector #1#) &key from-end (start 0) end count key)
    (with-predicate (predicate test)
      (with-key-function (key key)
        (delete-in-vector
         (lambda (x) (predicate (key x)))
         vector from-end start end count)))))

(seal-domain #'delete-if '(t vector))
