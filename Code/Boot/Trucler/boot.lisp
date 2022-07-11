(cl:in-package #:sicl-boot-trucler)

(defclass client (sicl-boot:client) ())

(defmacro with-temporary-function-imports
    (client-form environment-form names-form &body body)
  (let ((client-var (gensym))
        (environment-var (gensym))
        (names-var (gensym)))
    `(let ((,client-var ,client-form)
           (,names-var ,names-form)
           (,environment-var ,environment-form)
           functions)
       (unwind-protect
            (progn
              ;; Access the current definitions of the functions that
              ;; we need to be imported from the host, and store them
              ;; in the lexical variable FUNCTIONS in the same order
              ;; as their names appear in NAMES-FORM.  Note that the
              ;; current definition of some function could be NIL,
              ;; which means it is undefined in the environment.  What
              ;; then happens is that we restore it to be undefined at
              ;; the end.
              (setf functions
                    (loop for name in ,names-var
                          collect
                          (env:fdefinition ,client-var ,environment-var  name)))
              ;; Next, import the functions.
              (import-functions-from-host ,names-var ,environment-var)
              ;; Execute the body in the new context.
              ,@body)
         ;; Restore the original definitions of the functions.
         (loop for name in ,names-var
               for function in functions
               do (setf (env:fdefinition ,client-var ,environment-var name)
                        function))))))

(defparameter *host-function-names*
  '(;; Trucler needs the host FORMAT for (SETF DOCUMENTATION) at
    ;; compile time.
    format))

(defun boot (boot)
  (with-accessors ((e5 sicl-boot:e5))
      boot
    (let* ((client (make-instance 'client))
           (sicl-client:*client* client))
      (with-temporary-function-imports
          client e5 '(format)
        (ensure-asdf-system-using-client client e5 '#:trucler-base)
        (ensure-asdf-system-using-client client e5 '#:trucler-reference)))))
