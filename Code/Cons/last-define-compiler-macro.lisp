(cl:in-package #:sicl-cons)

;;; special version of last used when the second argument to last is
;;; equal to 1.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun last-1 (list)
    (unless (typep list 'list)
      (error 'must-be-list
             :datum list
             :name 'last))
    ;; We can use for ... on, because it uses atom to test for
    ;; the end of the list. 
    (loop for rest on list
          do (setf list rest))
    list))

(define-compiler-macro last (&whole form list &optional (n 1))
  (if (eql n 1)
      `(last-1 ,list)
      form))
