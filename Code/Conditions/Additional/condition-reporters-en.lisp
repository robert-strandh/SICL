(cl:in-package #:sicl-additional-conditions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Runtime conditions.

(defun interpret-type (type)
  (cond ((eq type 'cons)
         "a CONS cell")
        ((eq type 'list)
         "a list (NIL or a CONS cell)")
        ((eq type 'proper-list)
         "a proper list")
        ((eq type 'dotted-list)
         "a dotted list")
        ((eq type 'circular-list)
         "a circular list")
        ((equal type '(integer 0))
         "a non-negative integer")
        (t
         (format nil "an object of type ~s" type))))
