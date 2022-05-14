(cl:in-package #:sicl-boot-environment)

(defclass client (sicl-boot:client) ())

(defparameter *host-function-names*
  '(;; Clostrum needs SUBST at compile time.
    subst))

(defun boot (boot)
  (with-accessors ((e5 sicl-boot:e5))
      boot
    (let* ((client (make-instance 'client))
           (sicl-client:*client* client))
      (let (functions)
        (unwind-protect
             (progn
               ;; Access the current definitions in E5 of the
               ;; functions that we need to be imported from the host
               ;; during bootstrapping of the environment systems, and
               ;; store them in the lexical variable FUNCTIONS in the
               ;; same order as their names appear in
               ;; *HOST-FUNCTION-NAMES*.  Note that the current
               ;; definition of some function could be NIL, which
               ;; means it is undefined in E5.  What then happens is
               ;; that we restore it to be undefined after
               ;; bootstrapping.
               (setf functions
                     (loop for name in *host-function-names*
                           collect (env:fdefinition client e5 name)))
               ;; Next, import the functions named in
               ;; *HOST-FUNCTION-NAMES* into E5.
               (import-functions-from-host *host-function-names* e5)
               ;; Now, it should be safe to load Clostrum.
               (ensure-asdf-system-using-client client e5 '#:clostrum)
               (ensure-asdf-system-using-client client e5 '#:clostrum-basic)
               ;; And also SICL-ENVIRONMENT.
               (ensure-asdf-system-using-client client e5 '#:sicl-environment))
          ;; Restore the original definitions in E5 of the functions
          ;; named in *HOST-FUNCTION-NAMES*.
          (loop for name in *host-function-names*
                for function in functions
                do (setf (env:fdefinition client e5 name) function)))))))
