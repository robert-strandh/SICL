(cl:in-package #:sicl-boot-phase-1)

;;; We need to start using DEFMACRO early on to define macros, and
;;; since we don't already have it, we must create it "manually".
;;; This version is incorrect, though, because it uses the host
;;; compiler both to create the macro function for DEFMACRO (which is
;;; fine) and for creating the macro function for the macros defined
;;; by DEFMACRO (which is not fine).  As a result, the macros defined
;;; by this version of DEFMACRO must be defined in the NULL lexical
;;; environment.  Luckily, most macros are, and certainly the ones we
;;; need to define with this version of DEFMACRO until we can replace
;;; it with a native version.
;;;
;;; However, there is a different problem as well.  Since macro
;;; functions may call SICL-ENVIRONMENT:GLOBAL-ENVIRONMENT, it needs
;;; to be defined, but since the host compiler is used, the macro
;;; function is compiled in the null lexical environment, so
;;; SICL-ENVIRONMENT:GLOBAL-ENVIRONMENT is not defined.  To fix that
;;; problem, we wrap the macro body in a lexical definition of it that
;;; returns the run-time environment.
(defun define-defmacro (client environment)
  (setf (env:macro-function client environment 'defmacro)
        (compile nil
                 (cleavir-code-utilities:parse-macro
                  'defmacro
                  '(name lambda-list &body body)
                  `((eval-when (:compile-toplevel :load-toplevel :execute)
                      (setf (env:macro-function ,client ,environment name)
                            (compile nil
                                     (cleavir-code-utilities:parse-macro
                                      name
                                      lambda-list
                                      body)))))))))
