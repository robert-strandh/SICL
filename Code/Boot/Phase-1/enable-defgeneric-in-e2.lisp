(cl:in-package #:sicl-new-boot-phase-1)

;;; We want to make it possible to evaluate DEFGENERIC forms in
;;; environment E2 so that the result is the creation of a host
;;; standard generic function in E2.
;;;
;;; There are different ways in which we can accomplish this task,
;;; given the constraint that it has to be done by loading DEFGENERIC
;;; forms corresponding to the class accessor generic functions.
;;;
;;; We obviously can not use the host definition of DEFGENERIC because
;;; it might clobber any existing host definition.  In particular,
;;; this is the case for class accessor functions that have names in
;;; the COMMON-LISP package, for instance CLASS-NAME.  Since we must
;;; supply our own definition of DEFGENERIC, we are free to do what we
;;; want.
;;;
;;; The macro DEFGENERIC is particularly simple.  The macro itself
;;; calls the function DEFGENERIC-EXPANDER, passing it the environment
;;; in which it was invoked, the name of the generic function, the
;;; lambda list, and the options given.
;;;
;;; At macro-expansion time, the lambda list is converted to a type
;;; specifier by calling
;;; CLEAVIR-CODE-UTILITIES:LAMBDA-LIST-TYPE-SPECIFIER in order to
;;; derive a type for the function.  No processing is done at
;;; macro-expansion time that requires any other infrastructure.
;;; For that reason, we can have the host do the expansion.
;;; 
;;; The expansion computed by the expander has a compile-time part and
;;; a part that is executed load time and evaluation time.
;;;
;;; At compile time, SICL environment functions are called in order to
;;; assign a type and a lambda list to be associated with the function
;;; name.  These functions are given the environment in which the
;;; DEFGENERIC call was evaluated as explicit arguments.  Therefore,
;;; no environment lookup takes place at compile time.
;;;
;;; At load time and evaluation time, the environment is first
;;; determined.  This environment is then used to call
;;; ENSURE-GENERIC-FUNCTION, passing the name, the lambda list, and
;;; the environment as arguments.  For that reason,
;;; ENSURE-GENERIC-FUNCTION must exist in that environment, which in
;;; this case is E2.
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

(defun enable-defgeneric-in-e2 (e2)
  (import-function-from-host 'sicl-clos:defgeneric-expander e2)
  (load-file "CLOS/defgeneric-defmacro.lisp" e2)
  (setf (sicl-genv:fdefinition 'ensure-generic-function e2)
        #'ensure-generic-function-phase-1))
