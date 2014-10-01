(cl:in-package #:cleavir-ast-interpreter)

(defgeneric interpret-ast (ast static-env dynamic-env))

(defun interpret (ast)
  (let ((static-env (list (make-hash-table :test #'eq)))
	(dynamic-env '()))
    (interpret-ast ast static-env dynamic-env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utility function.

(defun interpret-sequence (sequence static-env dynamic-env)
  (loop for ast in sequence
	for value = (interpret-ast ast static-env dynamic-env)
	finally (return value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on INTERPRET-AST.

(defmethod interpret-ast ((ast cleavir-ast:constant-ast)
			  static-env dynamic-env)
  (declare (ignore static-env dynamic-env))
  (cleavir-ast:value ast))

(defmethod interpret-ast ((ast cleavir-ast:progn-ast)
			  static-env dynamic-env)
  (interpret-sequence (cleavir-ast:form-asts ast) static-env dynamic-env))

(defmethod interpret-ast ((ast cleavir-ast:setq-ast)
			  static-env dynamic-env)
  (set-lexical
   (cleavir-ast:lhs-ast ast)
   (interpret-ast (cleavir-ast:value-ast ast) static-env dynamic-env)
   static-env))

(defmethod interpret-ast ((ast cleavir-ast:lexical-ast)
			  static-env dynamic-env)
  (declare (ignore dynamic-env))
  (lookup-lexical ast static-env))

(defmethod interpret-ast ((ast cleavir-ast:symbol-value-ast)
			  static-env dynamic-env)
  (declare (ignore static-env))
  (lookup-special (cleavir-ast:symbol ast) dynamic-env))

(defmethod interpret-ast ((ast cleavir-ast:block-ast)
			  static-env dynamic-env)
  (let ((tag (list nil)))
    (set-lexical ast tag static-env)
    (catch tag
      (interpret-ast (cleavir-ast:body-ast ast) static-env dynamic-env))))

(defmethod interpret-ast ((ast cleavir-ast:return-from-ast)
			  static-env dynamic-env)
  (let* ((block (cleavir-ast:block-ast ast))
	 (tag (lookup-lexical block static-env))
	 (form-ast (cleavir-ast:form-ast ast)))
    (throw tag
      (interpret-ast form-ast static-env dynamic-env))))

(defmethod interpret-ast ((ast cleavir-ast:if-ast)
			  static-env dynamic-env)
  (if (interpret-ast (cleavir-ast:test-ast ast) static-env dynamic-env)
      (interpret-ast (cleavir-ast:then-ast ast) static-env dynamic-env)
      (interpret-ast (cleavir-ast:else-ast ast) static-env dynamic-env)))

(defmethod interpret-ast ((ast cleavir-ast:tag-ast)
			  static-env dynamic-env)
  (declare (ignore static-env dynamic-env))
  nil)

(defmethod interpret-ast ((ast cleavir-ast:go-ast)
			  static-env dynamic-env)
  (declare (ignore dynamic-env))
  (let* ((go-tag-ast (cleavir-ast:tag-ast ast))
	 (catch-tag (lookup-lexical go-tag-ast static-env)))
    (throw catch-tag go-tag-ast)))

(defmethod interpret-ast ((ast cleavir-ast:tagbody-ast)
			  static-env dynamic-env)
  (let ((catch-tag (list nil))
	(item-asts (cleavir-ast:item-asts ast)))
    ;; Start by entering the catch tag as the value of each go tags in
    ;; the tagbody in the lexical environment.
    (loop for item-ast in item-asts
	  when (typep item-ast 'cleavir-ast:tag-ast)
	    do (set-lexical item-ast catch-tag static-env))
    ;; The variable REMAINING-ITEM-ASTS holds a list of the items that
    ;; should be interpreted before the job is done.  Initially, it is
    ;; the list of all the items in the TAGBODY, but a GO will alter
    ;; that.
    (let ((remaining-item-asts item-asts))
      (flet ((do-item ()
	       ;; We know there is at least one remaining item-ast to
	       ;; interpret.
	       (interpret-ast (pop remaining-item-asts)
			      static-env dynamic-env)))
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

(defmethod interpret-ast ((ast cleavir-ast:fdefinition-ast)
			  static-env dynamic-env)
  (declare (ignore static-env dynamic-env))
  (fdefinition (cleavir-ast:name ast)))
