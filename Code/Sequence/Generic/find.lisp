(cl:in-package #:sicl-sequence)

(defmethod find (item (datum t) &key &allow-other-keys)
  (error 'must-be-sequence
         :datum datum))

(defmethod find (item (list list) &key from-end test test-not start end key)
  (with-test-function (test test test-not)
    (with-key-function (key key)
      (for-each-relevant-cons (cons index list start end from-end)
        (let ((element (car cons)))
          (when (test item (key element))
            (return-from find element)))))))

(replicate-for-each-relevant-vectoroid #1=#:vectoroid
  (defmethod find (item (vectoroid #1#) &key from-end test test-not start end key)
    (with-test-function (test test test-not)
      (with-key-function (key key)
        (for-each-relevant-element (element index vectoroid start end from-end)
          (when (test item (key element))
            (return-from find element)))))))
