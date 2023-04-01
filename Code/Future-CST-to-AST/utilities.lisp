(cl:in-package #:sicl-expression-to-ast)

(defun symbol-macro-expander (expansion)
  (lambda (form environment)
    (declare (ignore form environment))
    expansion))

(defun expand (expander form environment)
  (funcall (coerce *macroexpand-hook* 'function)
           expander form environment))

(defun expand-macro (expander cooked-form environment)
  (expand expander (c:raw cooked-form) environment))
