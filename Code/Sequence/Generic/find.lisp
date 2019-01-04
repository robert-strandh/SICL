(cl:in-package #:sicl-sequence)

(defmethod find ((item t) (datum t)  &key &allow-other-keys)
  (error 'must-be-sequence
         :datum datum))

(defmethod find ((item t) (list list) &key from-end test test-not start end key)
  (with-test-function (test test test-not)
    (with-key-function (key key)
      (for-each-relevant-cons (cons index list start end from-end)
        (let ((element (car cons)))
          (when (test item (key element))
            (return-from find element)))))))

(replicate-for-each-relevant-vector-subclass <vector-subclass>
  (defmethod find ((item t) (sequence <vector-subclass>) &key from-end test test-not start end key)
    (with-test-function (test test test-not)
      (with-key-function (key key)
        (for-each-relevant-element (element index sequence start end from-end)
          (when (test item (key element))
            (return-from find element)))))))

(defmethod find ((item t) (sequence sequence) &key from-end test test-not start end key)
  (check-type sequence (not vector))
  (with-test-function (test test test-not)
    (with-key-function (key key)
      (for-each-relevant-element (element index sequence start end from-end)
        (when (test item (key element))
          (return-from find element))))))
