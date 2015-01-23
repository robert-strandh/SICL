(cl:in-package #:sicl-sequence-test)

(defun position (item sequence &key from-end test test-not (start 0) end key)
  (sicl-sequence::position-aux
   item sequence from-end test test-not start end key))
