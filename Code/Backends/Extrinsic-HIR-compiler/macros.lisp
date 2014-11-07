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
