(cl:in-package #:cleavir-ast-interpreter)

(defgeneric interpret-ast (ast env))

(defun interpret (ast)
  (let ((env (list (make-hash-table :test #'eq)))
	(dynamic-env '()))
    (interpret-ast ast env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utility function.

(defun interpret-sequence (sequence env)
  (loop for ast in sequence
	for value = (interpret-ast ast env)
	finally (return value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on INTERPRET-AST.

(defmethod interpret-ast ((ast cleavir-ast:constant-ast) env)
  (declare (ignore env))
  (cleavir-ast:value ast))

(defmethod interpret-ast ((ast cleavir-ast:progn-ast) env)
  (interpret-sequence (cleavir-ast:form-asts ast) env))

(defmethod interpret-ast ((ast cleavir-ast:setq-ast) env)
  (set-lexical
   (cleavir-ast:lhs-ast ast)
   (interpret-ast (cleavir-ast:value-ast ast) env)
   env))

(defmethod interpret-ast ((ast cleavir-ast:lexical-ast) env)
  (lookup-lexical ast env))

(defmethod interpret-ast ((ast cleavir-ast:symbol-value-ast) env)
  (declare (ignore env))
  (symbol-value (cleavir-ast:symbol ast)))

(defmethod interpret-ast ((ast cleavir-ast:block-ast) env)
  (let ((tag (list nil)))
    (set-lexical ast tag env)
    (catch tag
      (interpret-ast (cleavir-ast:body-ast ast) env))))

(defmethod interpret-ast ((ast cleavir-ast:return-from-ast) env)
  (let* ((block (cleavir-ast:block-ast ast))
	 (tag (lookup-lexical block env))
	 (form-ast (cleavir-ast:form-ast ast)))
    (throw tag
      (interpret-ast form-ast env))))

(defmethod interpret-ast ((ast cleavir-ast:if-ast) env)
  (if (interpret-ast (cleavir-ast:test-ast ast) env)
      (interpret-ast (cleavir-ast:then-ast ast) env)
      (interpret-ast (cleavir-ast:else-ast ast) env)))

(defmethod interpret-ast ((ast cleavir-ast:tag-ast) env)
  (declare (ignore env))
  nil)

(defmethod interpret-ast ((ast cleavir-ast:go-ast) env)
  (let* ((go-tag-ast (cleavir-ast:tag-ast ast))
	 (catch-tag (lookup-lexical go-tag-ast env)))
    (throw catch-tag go-tag-ast)))

(defmethod interpret-ast ((ast cleavir-ast:tagbody-ast) env)
  (let ((catch-tag (list nil))
	(item-asts (cleavir-ast:item-asts ast)))
    ;; Start by entering the catch tag as the value of each go tags in
    ;; the tagbody in the lexical environment.
    (loop for item-ast in item-asts
	  when (typep item-ast 'cleavir-ast:tag-ast)
	    do (set-lexical item-ast catch-tag env))
    ;; The variable REMAINING-ITEM-ASTS holds a list of the items that
    ;; should be interpreted before the job is done.  Initially, it is
    ;; the list of all the items in the TAGBODY, but a GO will alter
    ;; that.
    (let ((remaining-item-asts item-asts))
      (flet ((do-item ()
	       ;; We know there is at least one remaining item-ast to
	       ;; interpret.
	       (interpret-ast (pop remaining-item-asts)
			      env)))
	(flet ((do-items ()
		 ;; Interpret all remaining items.
		 (loop when (null remaining-item-asts)
			 ;; If there are no more items to interpret,
			 ;; this means whe have reached the end of the
			 ;; TAGBODY, so we are done interpreting it.
			 do (return-from interpret-ast nil)
		       do (do-item))))
	  ;; Each iteration of this loop corresponds to a GO.  If
	  ;; there is no GO in a particular iteration of this loop,
	  ;; then the local function DO-ITEMS will interpret all the
	  ;; remaining items and return from this method.  If there is
	  ;; a GO during the interpretation of one of the items, then
	  ;; the execution of DO-ITEMS will be interrupted by a THROW
	  ;; with the GO-TAG-AST as its value.  We CATCH that event
	  ;; and set the remaining items to the list of items starting
	  ;; with that GO-TAG-AST.
	  (loop do (let ((go-tag-ast (catch catch-tag (do-items))))
		     (setf remaining-item-asts
			   (member go-tag-ast item-asts)))))))))

(defmethod interpret-ast ((ast cleavir-ast:fdefinition-ast) env)
  (declare (ignore env))
  (fdefinition (cleavir-ast:name ast)))

(defclass interpreted-function ()
  ((%environment :initarg :environment :reader environment)
   (%lambda-list :initarg :lambda-list :reader lambda-list)
   (%body-ast :initarg :body-ast :reader body-ast)))

(defmethod interpret-ast ((ast cleavir-ast:function-ast) env)
  (make-instance 'interpreted-function
    :environment env
    :lambda-list (cleavir-ast:lambda-list ast)
    :body-ast (cleavir-ast:body-ast ast)))

(defun interpret-primitive-call (callee arguments)
  (apply callee arguments))

(defun interpret-interpreted-call (callee arguments)
  (declare (ignore callee arguments))
  (error "not yet implemented"))

(defmethod interpret-ast ((ast cleavir-ast:call-ast) env)
  (let ((callee (interpret-ast (cleavir-ast:callee-ast ast)
			       env))
	(args (loop for arg-ast in (cleavir-ast:argument-asts ast)
		    collect (interpret-ast arg-ast env))))
    (if (typep callee 'function)
	(interpret-primitive-call callee args)
	(interpret-interpreted-call callee args))))
