(cl:in-package #:sicl-ast-evaluator)

(defun augment-environment (environment lambda-list)
  (let ((table (make-hash-table :test #'eq)))
    (loop for item in lambda-list
          do (cond ((member item lambda-list-keywords)
                    nil)
                   ((atom item)
                    (setf (gethash item table) (gensym)))
                   ((= (length item) 2)
                    ;; It is an &OPTIONAL parameter constellation.
                    (setf (gethash (first item) table) (gensym))
                    (setf (gethash (second item) table) (gensym)))
                   (t
                    ;; It is a &KEY parameter constellation.
                    (setf (gethash (second item) table) (gensym))
                    (setf (gethash (third item) table) (gensym)))))
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
