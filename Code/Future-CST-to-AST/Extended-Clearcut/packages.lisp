(cl:in-package #:common-lisp-user)

(defpackage #:sicl-extended-clearcut
  (:use #:clearcut)
  (:import-from
   #:common-lisp
   #:defun
   #:defgeneric
   #:defmethod)
  (:export
   . #.(loop with clearcut = (find-package '#:clearcut)
             for symbol being each external-symbol in clearcut
             collect (symbol-name symbol)))
  (:export #:origin))

