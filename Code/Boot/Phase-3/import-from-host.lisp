(cl:in-package #:sicl-boot-phase-3)

(defun import-number-functions (e3)
  (import-functions-from-host
   '(+ - < <= = /= floor 1+ 1-
     ;; ZEROP is used in CLOS to compute the discriminating function.
     zerop
     ;; RANDOM is used to create a hash code for standard
     ;; objects.
     random)
   e3))

(defun import-code-utilities (e3)
  (import-functions-from-host
   '(cleavir-code-utilities:proper-list-p
     cleavir-code-utilities:extract-required
     cleavir-code-utilities:canonicalize-generic-function-lambda-list)
   e3))

(defun import-misc (e3)
  (import-functions-from-host
   '(slot-unbound
     no-applicable-method
     sicl-method-combination:define-method-combination-expander
     shared-initialize initialize-instance reinitialize-instance
     closer-mop:method-function
     sicl-clos:parse-defmethod sicl-clos:canonicalize-specializers
     (setf env:macro-function)
     coerce)
   e3)
  (setf (env:macro-function (env:client e3) e3 'defpackage)
        (lambda (form env)
          (declare (ignore env))
          (eval form)
          nil)))

(defun import-data-and-control-flow (e3)
  (import-functions-from-host
   '(not eq eql equal values)
   e3))

(defun import-from-host (e3)
  (import-number-functions e3)
  (import-code-utilities e3)
  (import-misc e3)
  (import-data-and-control-flow e3))
