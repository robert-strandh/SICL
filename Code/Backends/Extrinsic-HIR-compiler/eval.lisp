(cl:in-package #:sicl-extrinsic-hir-compiler)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Main entry point.

(defparameter *fdefinition*
  (lambda (name) (sicl-env:fdefinition name *environment*)))

(defun eval (form)
  (let* ((ast (cleavir-generate-ast:generate-ast form *environment*))
	 (hir (cleavir-ast-to-hir:compile-toplevel ast)))
    (funcall
     (funcall 
      (compile
       nil
       `(lambda ()
	  (let ((fdefinition *fdefinition*))
	    (declare (ignorable fdefinition))
	    (lambda () ,(translate hir)))))))))
