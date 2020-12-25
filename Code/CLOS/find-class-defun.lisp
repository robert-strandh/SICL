(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function FIND-CLASS.
;;;
;;; This function is defined the way it is because we want it to be
;;; defined in an environment where we will ultimately undefine
;;; functions with names in the SICL-ENVIRONMENT package.  So the
;;; function SICL-ENVIRONMENT:FIND-CLASS and the CLIENT object of the
;;; environment are closed over at load time so that they can be
;;; undefined after this definition has been loaded.

(let* ((environment (sicl-environment:global-environment))
       (client (sicl-environment:client environment))
       (find-class
         (fdefinition 'sicl-environment:find-class)))
  (defun find-class (symbol &optional (errorp t) environment)
    (let ((class (funcall find-class client environment symbol)))
      (if (and (null class) errorp)
          (error 'sicl-clos:no-such-class-name :name symbol)
          class))))
