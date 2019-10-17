(cl:in-package #:sicl-boot-phase-6)

(defun load-data-and-control-flow-code (e5)
  (load-fasl "Data-and-control-flow/eq-defun.fasl" e5)
  (load-fasl "Data-and-control-flow/defun-defmacro.fasl" e5)
  (load-fasl "Data-and-control-flow/fdefinition-defun.fasl" e5)
  (load-fasl "Data-and-control-flow/setf-fdefinition-defun.fasl" e5))
