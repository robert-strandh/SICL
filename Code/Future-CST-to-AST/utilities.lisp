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

(defun expand-compiler-macro (expander cooked-form environment)
  (let ((raw-form (c:raw cooked-form)))
    (restart-case
        (expand expander raw-form environment)
      (continue ()
        :report "Ignore compiler macro."
        (return-from expand-compiler-macro raw-form)))))

(defun convert-sequence (client cooked-sequence environment)
  (loop for remaining = cooked-sequence then (c:rest remaining)
        until (c:null cooked-sequence)
        collect (convert client (c:first remaining) environment)))
