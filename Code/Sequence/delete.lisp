(cl:in-package #:sicl-sequence)

(defmethod delete (item (list list) &key from-end test test-not (start 0) end count key)
  (with-test-function (test test test-not)
    (with-key-function (key key)
      (delete-in-list
       (lambda (x) (test item (key x)))
       list from-end start end count))))

(seal-domain #'delete '(t list))

(replicate-for-each-vector-class #1=#:vector-class
  (defmethod delete (item (vector #1#) &key from-end test test-not (start 0) end count key)
    (with-test-function (test test test-not)
      (with-key-function (key key)
        (delete-in-vector
         (lambda (x) (test item (key x)))
         vector from-end start end count)))))

(seal-domain #'delete '(t vector))
