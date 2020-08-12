(cl:in-package #:sicl-boot-phase-8)

(defun load-sicl-utilities (environment)
  (sicl-boot-phase-0::host-load "Utilities/packages.lisp")
  (setf (sicl-genv:constant-variable 'most-positive-fixnum environment)
        (1- (expt 2 62)))
  (import-function-from-host 'cleavir-code-utilities:parse-deftype environment)
  (load-asdf-system-components '#:sicl-utilities environment))

