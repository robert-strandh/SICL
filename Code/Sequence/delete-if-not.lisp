(cl:in-package #:sicl-sequence)

(defmethod delete-if-not (test (list list) &key from-end (start 0) end count key)
  (with-predicate (predicate test)
    (with-key-function (key key)
      (delete-in-list
       (lambda (x) (not (predicate (key x))))
       list from-end start end count))))

(seal-domain #'delete-if-not '(t list))

(replicate-for-each-vector-class #1=#:vector-class
  (defmethod delete-if-not (test (vector #1#) &key from-end (start 0) end count key)
    (with-predicate (predicate test)
      (with-key-function (key key)
        (delete-in-vector
         (lambda (x) (not (predicate (key x))))
         vector from-end start end count)))))

(seal-domain #'delete-if-not '(t vector))
