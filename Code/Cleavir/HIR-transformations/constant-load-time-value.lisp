(cl:in-package #:cleavir-hir-transformations)

;;; Return true if and only if the LOAD-TIME-VALUE-INPUT represents a
;;; constant.  We consider it to be a constant only if the FORM of the
;;; LOAD-TIME-VALUE-INPUT has a FORM of the form (QUOTE constant).
(defun load-time-value-is-constant-p (load-time-value-input)
  (check-type load-time-value-input cleavir-ir:load-time-value-input)
  (let ((form (cleavir-ir:form load-time-value-input)))
    (and (consp form)
	 (consp (rest form))
	 (null (rest (rest form)))
	 (eq (first form) 'quote))))
