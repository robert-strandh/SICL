(cl:in-package #:sicl-boot-phase-2)

;;; We want to make it possible to load FASL files containing
;;; definitions of generic functions in E3 so that the result is the
;;; creation of a host standard generic function in E3.
;;;
;;; Defining a generic function involves calling
;;; ENSURE-GENERIC-FUNCTION, passing the name, the lambda list, and
;;; the environment as arguments.  For that reason,
;;; ENSURE-GENERIC-FUNCTION must exist in that environment, which in
;;; this case is E3.
;;;
;;; The way we have chosen to do it is to provide a specific
;;; definition of ENSURE-GENERIC-FUNCTION.  We do not want to use the
;;; ordinary SICL version of ENSURE-GENERIC-FUNCTION because it
;;; requires a battery of additional functionality in the form of
;;; other generic functions.  So to keep things simple, we supply a
;;; special bootstrapping version of it.
;;;
;;; We can rely entirely on the host to execute the generic-function
;;; initialization protocol.

(defun ensure-generic-function-phase-1
    (function-name &rest arguments &key environment &allow-other-keys)
  (let ((args (copy-list arguments)))
    (loop while (remf args :environment))
    (if (sicl-genv:fboundp function-name environment)
        (sicl-genv:fdefinition function-name environment)
        (setf (sicl-genv:fdefinition function-name environment)
              (apply #'make-instance 'standard-generic-function
                     :name function-name
                     :method-combination
                     (closer-mop:find-method-combination
                      #'class-name 'standard '())
                     args)))))

(defun enable-defgeneric (e3)
  (setf (sicl-genv:fdefinition 'ensure-generic-function e3)
        #'ensure-generic-function-phase-1))
