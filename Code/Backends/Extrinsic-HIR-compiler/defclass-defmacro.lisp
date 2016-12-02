(cl:in-package #:sicl-extrinsic-hir-compiler)

(defmacro defclass (name superclasses slots)
  (let ((host-name (gensym)))
    `(progn (host-common-lisp:eval
	     '(defclass ,host-name ,superclasses
	       ,(loop for slot in slots
		      collect
		      `(,(first slot)
			,@(loop for (option-name option-value)
				  on (rest slot) by #'cddr
				unless (member option-name
					       '(:reader :writer :acessor))
				  append (list option-name option-value))))))
	  (setf (find-class ',name)
		(host-common-lisp:find-class ',host-name)))))



			      
			      
		    
