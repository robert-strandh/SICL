(in-package #:sicl-compiler)

(defvar *backend*)

(defun compile-lambda-expression (lambda-expression)
  (let* ((ast (sicl-compiler-phase-1:convert-top-level-lamda-expression
	       lambda-expression))
	 (graph (sicl-compiler-phase-2:compile-toplevel ast))
	 (program (make-instance 'sicl-program:program
		    :initial-instruction graph
		    :backend *backend*)))
    (sicl-program:initial-transformations program)
    program))
    
