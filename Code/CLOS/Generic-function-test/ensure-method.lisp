(cl:in-package #:sicl-clos)

(defun ensure-method (generic-function
		      &rest keys
		      &key specializers
		      &allow-other-keys)
  (unless (cleavir-code-utilities:proper-list-p specializers)
    (error "Specializers must be a proper list: ~s" specializers))
  (let ((specs (loop for s in specializers
		     collect (find-class s)))
	(remaining-keys (copy-list keys)))
    (loop while (remf remaining-keys :specializers))
    (let ((method (make-method-for-generic-function
                   generic-function specs remaining-keys)))
      (push method (generic-function-methods generic-function))
      method)))
