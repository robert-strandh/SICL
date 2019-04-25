(cl:in-package #:cleavir-ast-visualizer)

(defgeneric background-color (ast))

(defmethod background-color (ast)
  clim:+white+)

(defmethod background-color ((ast cleavir-ast:constant-ast))
  clim:+pink+)

(defun compute-dynamic-environment-colors (ast)
  (let ((visited (make-hash-table :test #'eq))
        (colors (make-hash-table :test #'eq))
        (choices (list clim:+red+ clim:+blue+ clim:+green+
                       clim:+cyan+ clim:+magenta+ clim:+gold+)))
    ;; Turn the choices into a circular list.
    (setf (cdr (last choices)) choices)
    (flet ((assign-color (dynamic-environment-ast)
             (when (null (gethash dynamic-environment-ast colors))
               (setf (gethash dynamic-environment-ast colors)
                     (pop choices)))))
      (labels ((visit (ast)
                 (unless (gethash ast visited)
                   (when (typep ast 'cleavir-ast:dynamic-environment-input-ast-mixin)
                     (assign-color (cleavir-ast:dynamic-environment-input-ast ast)))
                   (when (typep ast 'cleavir-ast:dynamic-environment-output-ast-mixin)
                     (assign-color (cleavir-ast:dynamic-environment-output-ast ast)))
                   (loop for child in (cleavir-ast:children ast)
                         do (visit child)))))
        (visit ast)))
    colors))
