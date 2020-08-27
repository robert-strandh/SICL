(cl:in-package #:sicl-evaluation-and-compilation)

(defmacro defmacro (&environment environment name lambda-list &body body)
  (let ((compilation-environment
          (sicl-environment:global-environment environment))
        (expansion
          (cleavir-code-utilities:parse-macro name lambda-list body)))
    `(progn (eval-when (:compile-toplevel)
              (setf (clostrum:macro-function
                     (client ,compilation-environment)
                     ,compilation-environment
                     ',name)
                    ,expansion))
            (eval-when (:load-toplevel :execute)
              (let* ((run-time-environment
                       (sicl-environment:global-environment))
                     (client
                       
