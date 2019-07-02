(cl:in-package #:sicl-boot-phase-0)

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
(defun define-defmacro (environment)
  (setf (sicl-genv:macro-function 'defmacro environment)
        (compile nil
                 (cleavir-code-utilities:parse-macro
                  'defmacro
                  '(name lambda-list &body body)
                  `((eval-when (:compile-toplevel :load-toplevel :execute)
                      (setf (sicl-genv:macro-function name ,environment)
                            (compile nil
                                     (cleavir-code-utilities:parse-macro
                                      name
                                      lambda-list
                                      body)))))))))
