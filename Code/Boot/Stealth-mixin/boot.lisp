(cl:in-package #:sicl-boot-stealth-mixin)

(defclass client (sicl-boot:client) ())

(defmethod eclector.reader:interpret-symbol :around
    ((client client) input-stream package-indicator symbol-name internp)
  (if (and (stringp package-indicator)
           (string= package-indicator "CLOSER-MOP"))
      (identity (find-symbol symbol-name (find-package '#:sicl-clos)))
      (call-next-method)))

;;; It is possible that we don't need to import any functions from the
;;; host in order to bootstrap STEALTH-MIXIN, but we keep this code in
;;; case the situation changes later on.
(defparameter *host-function-names*
  '())

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
               ;; during bootstrapping, and store them in the lexical
               ;; variable FUNCTIONS in the same order as their names
               ;; appear in *HOST-FUNCTION-NAMES*.  Note that the
               ;; current definition of some function could be NIL,
               ;; which means it is undefined in E5.  What then
               ;; happens is that we restore it to be undefined after
               ;; bootstrapping.
               (setf functions
                     (loop for name in *host-function-names*
                           collect (env:fdefinition client e5 name)))
               ;; Next, import the functions named in
               ;; *HOST-FUNCTION-NAMES* into E5.
               (import-functions-from-host *host-function-names* e5)
               ;; Now, it should be safe to load the system
               (ensure-asdf-system-using-client client e5 '#:stealth-mixin))
          ;; Restore the original definitions in E5 of the functions
          ;; named in *HOST-FUNCTION-NAMES*.
          (loop for name in *host-function-names*
                for function in functions
                do (setf (env:fdefinition client e5 name) function)))))))
