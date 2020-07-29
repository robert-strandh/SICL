(cl:in-package #:sicl-conditions)

;;; This definition is taken directly from the "Notes" section of the
;;; dictionary entry for BREAK in the Common Lisp standard.

(defun break (&optional (format-control "Break") &rest format-arguments)
  (with-simple-restart (continue "Return from BREAK.")
    (let ((*debugger-hook* nil))
      (invoke-debugger
       (make-condition 'simple-condition
                       :format-control format-control
                       :format-arguments format-arguments))))
  nil)
