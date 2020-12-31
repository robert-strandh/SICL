(cl:in-package #:common-lisp-user)

(defpackage #:sicl-data-and-control-flow
  (:use #:common-lisp)
  (:export #:shiftf-expander
           #:defun-expander
           #:psetf-expander
           #:rotatef-expander
           #:setf-expander
           #:destructuring-bind-expander
           #:special-variable
           #:constant-variable
           #:function-cell))
