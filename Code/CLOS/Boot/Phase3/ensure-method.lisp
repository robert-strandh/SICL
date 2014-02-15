(in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ENSURE-METHOD.
;;;
;;; During bootstrapping, we know that the specializers are class
;;; names.  Furthermore, during phase 3 of the bootstrapping process,
;;; we know that the classes of those specializers can be found by
;;; using FIND-TARGET-CLASS.

;;; Since at this point ENSURE-METHOD is currently fbound to the
;;; version used in phase 2, we start by making it funbound so as to
;;; avoid compiler warnings.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (fmakunbound 'ensure-method))

(defun ensure-method (generic-function
		      &rest keys
		      &key specializers
		      &allow-other-keys)
  (let ((specs (loop for s in specializers
		     collect (find-target-class s)))
	(remaining-keys (copy-list keys)))
    (loop while (remf remaining-keys :specializers))
    (let ((method (apply #'make-instance
			 'standard-method
			 :specializers specs
			 remaining-keys)))
      (add-method generic-function method)
      method)))
