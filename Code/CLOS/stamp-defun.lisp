(cl:in-package #:sicl-clos)

;;; The implementation of this function is not complete.  Furthermore,
;;; this is probably not a good location for it.
(defun stamp (instance)
  (if (general-instance-p instance)
      (cleavir-primop:nook-read instance 0)
      ;; For now, anything else is considered to be an instance of
      ;; class T, and we know that T has unique number 0.
      0))
