(cl:in-package #:sicl-evaluation-and-compilation)

(defmacro defmacro (&environment environment name lambda-list &body body)
  (let ((compilation-environment
          (sicl-environment:global-environment environment))
        (expansion
          (cleavir-code-utilities:parse-macro name lambda-list body)))
    `(progn (eval-when (:compile-toplevel)
              (setf (sicl-environment:macro-function
                     (sicl-environment:client ,compilation-environment)
                     ,compilation-environment
                     ',name)
                    ,expansion))
            (eval-when (:load-toplevel :execute)
              (setf (sicl-environment:macro-function
                     (sicl-environment:client run-time-environment)
                     (sicl-environment:global-environment)
                     ',name)
                    ,expansion)))))
