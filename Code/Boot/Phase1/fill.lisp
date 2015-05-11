(cl:in-package #:sicl-boot-phase1)

(defun ld (filename environment)
  (format *trace-output* "Loading file ~a~%" filename)
  (finish-output *trace-output*)
  (sicl-extrinsic-environment:load-source-with-environments
   filename (compilation-environment environment) environment))

(defun fill-environment (environment)
  (sicl-genv:fmakunbound 'sicl-clos:ensure-generic-function-using-class
			 environment)
  (ld "../../CLOS/ensure-generic-function-using-class-defgenerics.lisp"
      environment)
  (ld "../../CLOS/ensure-generic-function-using-class-support.lisp"
      environment)
  (ld "../../CLOS/ensure-generic-function-using-class-defmethods.lisp"
      environment)
  (ld "../../CLOS/ensure-class-using-class-support.lisp"
      environment)
  (ld "../../CLOS/ensure-class-using-class-defgenerics.lisp"
      environment)
  (ld "../../CLOS/ensure-class-using-class-defmethods.lisp"
      environment)
  (ld "../../CLOS/ensure-class.lisp" environment)
  (ld "../../Environment/standard-environment-functions.lisp" environment)
  (setf (sicl-genv:find-class 'standard-object environment) nil)
  (ld "../../CLOS/standard-object-defclass.lisp" environment)
  (setf (sicl-genv:find-class 'sicl-clos:metaobject environment) nil)
  (ld "../../CLOS/metaobject-defclass.lisp" environment))
