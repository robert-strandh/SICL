(cl:in-package #:sicl-cons)

(defun maplist (function &rest lists)
  (when (null lists)
    (error 'at-least-one-list-required :name 'maplist))
  (loop for remaining = lists
          then (loop for list in remaining collect (cdr list))
        until (loop for list in remaining thereis (atom list))
        collect (apply function remaining)
        finally (loop for rem in remaining
                      for list in lists
                      do (when (not (listp rem))
                           (error 'must-be-proper-list
                                  :datum list
                                  :name 'maplist)))))
