(cl:in-package #:sicl-symbol)

(defun make-symbol (string)
  (unless (stringp string)
    (error 'type-error
           :datum string
           :expected-type 'string))
  (make-instance 'symbol
    :name string
    :package nil))
