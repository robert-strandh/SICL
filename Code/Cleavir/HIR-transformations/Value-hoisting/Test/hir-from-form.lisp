(cl:in-package #:cleavir-value-hoisting-test)

(defun hir-funcall (lexical-location &rest constants)
  (cleavir-ir:make-enter-instruction
   '()
   (cleavir-ir:dynamic-environment-location lexical-location)
   :successor
   (make-instance 'cleavir-ir:funcall-instruction
     :inputs (list* lexical-location (mapcar #'cleavir-ir:make-constant-input constants))
     :successor
     (make-instance 'cleavir-ir:return-instruction))))

;;; This :AROUND method replaces certain forms with a custom expansion.  In
;;; doing so, we avoid circularities such as when the creation form of a
;;; symbol consists of symbols.
(defmethod hir-from-form :around ((form cons) (client client) environment)
  (case (first form)
    (%make-string (apply #'hir-funcall *make-string* (rest form)))
    (%cons (hir-funcall *cons*))
    (%find-package (hir-funcall *find-package* (second form)))
    (%intern (hir-funcall *intern* (second form) (third form)))
    (%function-cell (hir-funcall *function-cell* (second form)))
    (otherwise (call-next-method))))

(defmethod hir-from-form (form (client client) environment)
  (cleavir-ast-to-hir:compile-toplevel
   (cleavir-cst-to-ast:cst-to-ast
    client
    (cst:cst-from-expression `(lambda () ,form))
    environment)))
