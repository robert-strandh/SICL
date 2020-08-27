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
    (list* (make-hash-table :test #'eq) table environment)))

(defun find-identifier (environment ast)
  (loop for table in environment
        do (multiple-value-bind (name presentp)
               (gethash ast table)
             (when presentp (return name)))
        finally (let ((name (gensym)))
                  (setf (gethash ast (first environment)) name)
                  (return name))))

(defun add-identifier (environment ast &optional (identifier (gensym)))
  (setf (gethash ast (first environment))
        identifier))
