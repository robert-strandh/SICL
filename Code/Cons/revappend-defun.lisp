(cl:in-package #:sicl-cons)

(defun revappend (list tail)
  (loop with result = tail
        for remaining = list then (cdr remaining)
        until (atom remaining)
        do (push (car remaining) result)
        finally (unless (null remaining)
                  (error 'must-be-proper-list
                         :datum list
                         :name 'revappend))
                (return result)))
