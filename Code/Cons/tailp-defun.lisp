(cl:in-package #:sicl-cons)

;;; We could use (loop for ... on ...) here for consistency.

(defun tailp (object list)
  (loop for rest = list then (cdr rest)
        until (atom rest)
        when (eql object rest)
          return t
        finally (return (eql object rest))))
