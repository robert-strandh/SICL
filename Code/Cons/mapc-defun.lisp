(cl:in-package #:sicl-cons)

(defun mapc (function &rest lists)
  (when (null lists)
    (error 'at-least-one-list-required :name 'mapc))
  (loop for remaining = lists
          then (loop for list in remaining collect (cdr list))
        until (loop for list in remaining thereis (atom list))
        do (apply function
                  (loop for list in remaining collect (car list)))
        finally (loop for rem in remaining
                      for list in lists
                      do (when (not (listp rem))
                           (error 'must-be-proper-list
                                  :datum list
                                  :name 'mapc))))
  ;; The mapc function returns the first list.
  (car lists))
