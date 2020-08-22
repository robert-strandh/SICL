(cl:in-package #:sicl-ast-evaluator)

(defun augment-environment (environment)
  (cons (make-hash-table :test #'eq)
        environment))

(defun find-lexical-variable (environment variable-ast)
  (loop for table in environment
        do (multiple-value-bind (name presentp)
               (gethash variable-ast table)
             (when presentp (return name)))
        finally (let ((name (gensym)))
                  (setf (gethash variable-ast (first environment)) name)
                  (return name))))

               
