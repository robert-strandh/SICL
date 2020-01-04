(cl:in-package #:sicl-sequence)

(defmethod delete (item (datum t) &key &allow-other-keys)
  (error 'must-be-sequence
         :datum datum))

(defmethod delete (item (list list) &key from-end test test-not start end count key)
  (with-test-function (test test test-not)
    (with-key-function (key key)
      (delete-in-list
       (lambda (x) (test item (key x)))
       list from-end start end count))))

(replicate-for-each-relevant-vectoroid #1=#:vectoroid
  (defmethod delete (item (vectoroid #1#) &key from-end test test-not start end count key)
    (with-test-function (test test test-not)
      (with-key-function (key key)
        (delete-in-vector
         (lambda (x) (test item (key x)))
         vectoroid from-end start end count)))))
