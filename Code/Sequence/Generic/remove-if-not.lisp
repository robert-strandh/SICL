(cl:in-package #:sicl-sequence)

(defmethod remove-if-not (test (list list) &key from-end (start 0) end count key)
  (with-predicate (predicate test)
    (with-key-function (key key)
      (remove-from-list
       (lambda (element) (not (predicate (key element))))
       list from-end start end count))))

(seal-domain #'remove-if-not '(t list))

(replicate-for-each-vector-class #1=#:vector-class
  (defmethod remove-if-not (test (vector #1#) &key from-end (start 0) end count key)
    (with-predicate (predicate test)
      (with-key-function (key key)
        (remove-from-vector
         (lambda (element) (not (predicate (key element))))
         vector from-end start end count)))))

(seal-domain #'remove-if-not '(t vector))
