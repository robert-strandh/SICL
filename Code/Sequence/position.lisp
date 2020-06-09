(cl:in-package #:sicl-sequence)

(defmethod position (item (list list) &key from-end test test-not (start 0) end key)
  (declare (method-properties inlineable))
  (with-test-function (test test test-not)
    (with-key-function (key key)
      (for-each-relevant-cons (cons index list start end from-end)
        (let ((element (car cons)))
          (when (test item (key element))
            (return-from position index)))))))

(seal-domain #'position '(t list))

(replicate-for-each-vector-class #1=#:vector-class
  (defmethod position (item (vector #1#) &key from-end test test-not (start 0) end key)
    (declare (method-properties inlineable))
    (with-test-function (test test test-not)
      (with-key-function (key key)
        (for-each-relevant-element (element index vector start end from-end)
          (when (test item (key element))
            (return-from position index)))))))

(seal-domain #'position '(t vector))
