(cl:in-package #:sicl-sequence)

(defmethod count (item (list list) &key from-end (start 0) end key test test-not)
  (declare (method-properties inlineable))
  (let ((count 0))
    (declare (list-length count))
    (with-test-function (test test test-not)
      (with-key-function (key key)
        (for-each-relevant-cons (cons index list start end from-end)
          (when (test item (key (car cons)))
            (incf count)))))
    count))

(seal-domain #'count '(t list))

(replicate-for-each-vector-class #1=#:vector-class
  (defmethod count (item (vector #1#) &key from-end (start 0) end key test test-not)
    (declare (method-properties inlineable))
    (let ((count 0))
      (declare (vector-length count))
      (with-test-function (test test test-not)
        (with-key-function (key key)
          (for-each-relevant-element (element index vector start end from-end)
            (when (test item (key element))
              (incf count)))))
      count)))

(seal-domain #'count '(t vector))
