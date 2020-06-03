(cl:in-package #:sicl-cons)

;;; This implementation of the LIST* function assumes that there is no
;;; structure sharing between the &REST argument and the last argument
;;; to apply
(defun list* (&rest elements)
  (when (null elements)
    (error 'at-least-one-argument-required))
  (if (null (cdr elements))
      (car elements)
      (loop for remaining on elements
            until (null (cddr remaining))
            finally (setf (cdr remaining)
                          (cadr remaining))
                    (return elements))))
