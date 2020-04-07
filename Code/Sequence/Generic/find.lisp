(cl:in-package #:sicl-sequence)

(defmethod find (item (list list) &key from-end test test-not (start 0) end key)
  (with-test-function (test test test-not)
    (with-key-function (key key)
      (for-each-relevant-cons (cons index list start end from-end)
        (let ((element (car cons)))
          (when (test item (key element))
            (return-from find element)))))))

(replicate-for-each-relevant-vectoroid #1=#:vectoroid
  (defmethod find (item (vectoroid #1#) &key from-end test test-not (start 0) end key)
    (with-test-function (test test test-not)
      (with-key-function (key key)
        (for-each-relevant-element (element index vectoroid start end from-end)
          (when (test item (key element))
            (return-from find element)))))))

(seal-domain #'find '(t sequence))
