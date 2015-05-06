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
      environment))
