(cl:in-package #:sicl-ast-evaluator)

(defun augment-environment (environment lambda-list)
  (let ((table (make-hash-table :test #'eq)))
    (loop for item in lambda-list
          do (cond ((member item lambda-list-keywords)
                    nil)
                   ((atom item)
                    (setf (gethash item table) (gensym)))
                   (t
                    (loop for var-ast in item
                          do (setf (gethash var-ast table) (gensym))))))
    (cons table environment)))

(defun find-lexical-variable (environment variable-ast)
  (loop for table in environment
        do (multiple-value-bind (name presentp)
               (gethash variable-ast table)
             (when presentp (return name)))
        finally (let ((name (gensym)))
                  (setf (gethash variable-ast (first environment)) name)
                  (return name))))

(defun add-block (environment block-ast)
  (setf (gethash block-ast (first environment))
        (gensym)))
