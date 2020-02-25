(cl:in-package #:sicl-run-time)

;;; This version is meant to be used with the HIR interpreter
(defun augment-with-block/tagbody-entry ()
  (let ((transfer-tag (list nil))
        (abandon-tag (list nil)))
    (values (cons (make-instance 'block/tagbody-entry
                    :frame-pointer abandon-tag)
                  *dynamic-environment*)
            transfer-tag)))

(defun augment-with-special-variable-entry (name value)
  (cons (make-instance 'special-variable-entry
          :name name
          :value value)
        *dynamic-environment*))
