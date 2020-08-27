(cl:in-package #:sicl-evaluation-and-compilation)

(defmacro defmacro (&environment environment name lambda-list &body body)
  (let ((compilation-environment
          (sicl-environment:global-environment environment))
        (expansion
          (cleavir-code-utilities:parse-macro name lambda-list body)))
    `(progn (eval-when (:compile-toplevel)
              (funcall #'(setf sicl-environment:macro-function)
                       ,expansion
                       (sicl-environment:client ,compilation-environment)
                       ,compilation-environment
                       ',name))
            (eval-when (:load-toplevel :execute)
              (funcall #'(setf sicl-environment:macro-function)
                       ,expansion
                       (sicl-environment:client (sicl-environment:global-environment))
                       (sicl-environment:global-environment)
                       ',name)))))
