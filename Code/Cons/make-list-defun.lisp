(cl:in-package #:sicl-cons)

(defun make-list (length &key (initial-element nil))
  (unless (and (integerp length) (>= length 0))
    (error 'must-be-nonnegative-integer
           :datum length
           :name 'make-list))
  (loop repeat length
        collect initial-element))
