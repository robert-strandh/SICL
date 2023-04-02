(cl:in-package #:sicl-expression-to-ast)

(defun test (form)
  (let ((clearcut:*client*
          (make-instance 'clearcut-implementation-s-expression:client))
        (environment
          (make-instance 'environment)))
    (expression-to-ast nil form environment)))
