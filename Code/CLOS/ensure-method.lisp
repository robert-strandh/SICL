(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ENSURE-METHOD.
;;;
;;; This function is not required by the AMOP to exist,
;;;
;;; One can think of this function as a convenience function that
;;; combines a call to MAKE-INSTANCE with a call to ADD-METHOD.  A
;;; SPECIALIZER, can be either a specializer meta object, or a symbol.
;;; If it is a symbol, it is considered to be the name of a class, and
;;; the call to MAKE-SPECIALZIER replaces the symbol with the
;;; corresponding class before calling MAKE-INSTANCE.

(defun ensure-method (generic-function
                      &rest keys
                      &key specializers
                      &allow-other-keys)
  (unless (cleavir-code-utilities:proper-list-p specializers)
    (error "Specializers must be a proper list: ~s" specializers))
  (let ((specs (loop for s in specializers
                     collect (make-specializer s)))
        (remaining-keys (copy-list keys)))
    (loop while (remf remaining-keys :specializers))
    (let ((method (make-method-for-generic-function
                   generic-function specs remaining-keys)))
      (add-method-to-generic-function generic-function method)
      method)))
