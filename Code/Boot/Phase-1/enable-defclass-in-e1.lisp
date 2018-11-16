(cl:in-package #:sicl-boot-phase-1)

(defun enable-defclass-in-e1 (boot)
  (with-accessors ((e1 sicl-boot:e1)) boot
    (import-function-from-host 'sicl-clos:defclass-expander e1)
    ;; This function is needed because we define a special variable
    ;; *CLASS-UNIQUE-NUMBER* that is used in the :INITFORM of the 
    ;; corresponding slot in the class CLASS.
    (import-function-from-host '(setf sicl-genv:special-variable) e1)))
