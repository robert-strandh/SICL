(cl:in-package #:sicl-global-environment)

(defmacro defun (name lambda-list &body body)
  (multiple-value-bind (declarations documentation forms)
      (cleavir-code-utilities:separate-function-body body)
    ;; FIXME: handle documentation
    (declare (ignore documentation))
    (let ((definition `(lambda ,lambda-list
			 ,declarations
			 (block ,name
			   ,@forms))))
      `(progn
	 (eval-when (:compile-toplevel)
	   (sicl-compiler:compile-time-compile ',name ',definition))
	 (eval-when (:load-toplevel :execute)
	   (funcall #'(setf fdefinition) ,definition ',name))))))
