(cl:in-package #:cleavir-compilation-policy)

(defun policy-value (policy quality)
  ;; policies are assumed to be complete
  (cdr (assoc quality policy)))
