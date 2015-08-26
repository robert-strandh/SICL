(cl:in-package #:cleavir-boolean-elimination)

;;; Return true if and only if INPUT is a LOAD-TIME-VALUE with a
;;; constant form in it, and if that constant is EQ to BOOLEAN.
(defun boolean-input-p (input boolean)
  (and (typep input 'cleavir-ir:load-time-value-input)
       (cleavir-hir-transformations:load-time-value-is-constant-p input)
       (eq boolean (cleavir-hir-transformations:load-time-value-constant input))))
