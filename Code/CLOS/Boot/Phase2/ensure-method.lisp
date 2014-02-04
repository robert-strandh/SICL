(in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ENSURE-METHOD.
;;;
;;; During bootstrap, we know that the specializers are class names.
;;; Furthermore, we know that the classes of those specializers are
;;; members of the association list *classes*.

(defun ensure-method (generic-function
		      &rest keys
		      &key specializers
		      &allow-other-keys)
  (let ((specs (loop for s in specializers
		     collect (cdr (assoc s *classes*))))
	(remaining-keys (copy-list keys)))
    (loop while (remf remaining-keys :specializers))
    (let ((method (apply #'make-instance
			 'standard-method
			 :specializers specs
			 remaining-keys)))
      (add-method generic-function method)
      method)))
