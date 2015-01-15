(in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ENSURE-METHOD.
;;;
;;; This function is not required by the AMOP to exist, 

(defun ensure-method (generic-function
		      &rest keys
		      &key specializers
		      &allow-other-keys)
  ;; FIXME: Check that SPECIALIZERS is a proper list, and that
  ;; it contains only symbols and specializer metaobjects. 
  (let ((specs (loop for s in specializers
		     collect (if (symbolp s) (find-class s) s)))
	(remaining-keys (copy-list keys)))
    (loop while (remf remaining-keys :specializers))
    (let ((method (apply #'make-instance
			 'standard-method
			 :specializers specs
			 remaining-keys)))
      (add-method generic-function method)
      method)))
