(cl:in-package #:sicl-boot-phase-6)

(defun enable-slot-value (e5)
  (setf (env:constant-variable
         (env:client e5) e5 'sicl-clos::+unbound-slot-value+)
        10000000)
  (load-source-file "CLOS/standard-instance-access.lisp" e5)
  (load-source-file "CLOS/slot-bound-using-index.lisp" e5)
  (load-source-file "CLOS/slot-value-etc-defgenerics.lisp" e5)
  (load-source-file "CLOS/slot-value-etc-support.lisp" e5)
  (load-source-file "CLOS/slot-value-etc-defmethods.lisp" e5)
  (load-source-file "CLOS/slot-value-etc-specified-defuns.lisp" e5))
  
