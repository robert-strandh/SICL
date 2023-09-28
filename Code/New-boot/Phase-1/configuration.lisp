(cl:in-package #:sicl-new-boot)

(defmethod cb:convert-with-parser-p ((client client) operator)
  (special-operator-p operator))

(defmethod cb:convert-with-ordinary-macro-p ((client client) operator)
  (cmd:macro-function-exists-p operator))

(defmethod cmd:defun-compile-time-action
    ((client client) name lambda-list environment)
  ;; FIXME: do something better
  234)
