(cl:in-package #:target-cons)

(defun mapcar (function list)
  (do ((sublist list (rest sublist))
       (result '() result))
      ((null sublist) (nreverse result))
    (push (funcall function (first sublist))
          result)))
