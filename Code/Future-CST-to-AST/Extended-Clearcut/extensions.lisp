(cl:in-package #:sicl-extended-clearcut)

(defgeneric origin* (client expression))

(defmethod origin*
  ((client clearcut-implementation-s-expression:client)
   expression)
  expression)

(defmethod origin*
  ((client clearcut-implementation-concrete-syntax-tree:client)
   expression)
  (cst:source expression))

(defun origin (expression)
  (origin* clearcut:*client* expression))
