(in-package #:sicl-boot-phase1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ENSURE-METHOD.
;;;
;;; During bootstrap, we know that the specializers are class names.
;;; Furthermore, we know that the classes of those specializers can
;;; be found by using FIND-BRIDGE-CLASS.

(defun ensure-method (generic-function
		      &rest keys
		      &key specializers
		      &allow-other-keys)
  (let ((specs (loop for s in specializers
		     collect (find-bridge-class s)))
	(remaining-keys (copy-list keys)))
    (loop while (remf remaining-keys :specializers))
    (let ((method (apply #'cl:make-instance
			 'standard-method
			 :specializers specs
			 remaining-keys)))
      (add-method generic-function method)
      method)))
