(cl:in-package #:sicl-extrinsic-hir-compiler)

(defmacro cl:catch (tag &body forms)
  (let ((block-name (gensym)))
    `(block ,block-name
       (catch tag
	 (lambda (values)
	   (return-from ,block-name
	     (apply #'values values)))
	 (lambda ()
	   ,@forms)))))

(defmacro cl:throw (tag result-form)
  `(throw tag
     (multiple-value-list ,result-form)))

(defmacro cl:unwind-protect (protected-form &body cleanup-forms)
  `(unwind-protect
     (lambda () ,protected-form)
     (lambda () ,@cleanup-forms)))
