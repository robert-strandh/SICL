(in-package #:sicl-procedure-integration)

(defun integrate-procedure-p (ast)
  (and (typep ast 'sicl-ast:call-ast)
       (typep (sicl-ast:callee-ast ast) 'sicl-ast:function-ast)))

(defun integrate-procedure (ast)
  (let* ((argument-asts (sicl-ast:argument-asts ast))
	 (callee-ast (sicl-ast:callee-ast ast))
	 (required (sicl-ast:required callee-ast))
	 (body (sicl-ast:body-ast callee-ast))
	 (assignments
	   (loop for arg in argument-asts
		 for loc in required
		 collect (sicl-ast:make-setq-ast loc arg))))
    (change-class ast
		  'sicl-ast:progn-ast
		  :form-asts (append assignments (list body)))))

(defun integrate-procedures (ast)
  (labels ((traverse (ast)
	     (when (integrate-procedure-p ast)
	       (integrate-procedure ast))
	     (mapc #'traverse (sicl-ast:children ast))))
    (traverse ast)))
