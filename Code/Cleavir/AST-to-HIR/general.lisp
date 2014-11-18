(cl:in-package #:cleavir-ast-to-hir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; During compilation, this variable contains a hash table that maps
;;; ASTs representing locations to HIR locations.

(defparameter *location-info* nil)

(defun find-or-create-location (ast)
  (or (gethash ast *location-info*)
      (let ((location
	      (etypecase ast
		(cleavir-ast:lexical-ast
		 (cleavir-ir:make-lexical-location
		  (cleavir-ast:name ast))))))
	(setf (gethash ast *location-info*) location))))

(defun make-temp (argument)
  (declare (ignore argument))
  (cleavir-ir:new-temporary))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile an abstract syntax tree in a compilation context.
;;;
;;; The result of the compilation is a single value, namely the first
;;; instruction of the instruction graph resulting from the
;;; compilation of the entire AST.

;;; Given a list of results and a successor, generate a sequence of
;;; instructions preceding that successor, and that assign NIL to each
;;; result in the list.
(defun nil-fill (results successor)
  (let ((next successor))
    (loop for value in results
	  do (setf next
		   (cleavir-ir:make-assignment-instruction
		    (cleavir-ir:make-constant-input 'nil)
		    value next))
	  finally (return next))))

(defgeneric compile-ast (ast context))

;;; When an AST that is meant for a test (as indicated by it being an
;;; instance of BOOLEAN-AST-MIXIN) is compiled in a context where one
;;; or more values are needed, we generate two branches; one where NIL
;;; is assigned to the first result and one where T is assigned to it
;;; (remaining results are filled with NIL).  Then we compile the AST
;;; in a context with the two branches and no result.
(defmethod compile-ast :around ((ast cleavir-ast:boolean-ast-mixin) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (ecase (length successors)
      (1
       (let* ((result (car results))
	      (successor (nil-fill (cdr results) (car successors)))
	      (true (cleavir-ir:make-constant-input T))
	      (false (cleavir-ir:make-constant-input NIL))
	      (true-branch (make-instance 'cleavir-ir:assignment-instruction
			     :inputs (list true)
			     :outputs (list result)
			     :successors (list successor)))
	      (false-branch (make-instance 'cleavir-ir:assignment-instruction
			      :inputs (list false)
			      :outputs (list result)
			      :successors (list successor))))
	 (call-next-method ast
			   (context '()
				    (list true-branch false-branch)
				    (invocation context)))))
      (2
       (call-next-method)))))

(defun check-context-for-boolean-ast (context)
  (assert (and (zerop (length (results context)))
	       (= (length (successors context)) 2))))

(defmethod compile-ast :around ((ast cleavir-ast:one-value-ast-mixin) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (ecase (length successors)
      (1
       ;; We have a context with one successor, so RESULTS can be a
       ;; list of any length, or it can be a values location,
       ;; indicating that all results are needed.
       (if (null results)
	   ;; We don't need the result.  This situation typically
	   ;; happens when we compile a form other than the last of a
	   ;; PROGN-AST.
	   (if (cleavir-ast:side-effect-free-p ast)
	       (progn
		 ;; Warn an generate no code.
		 (warn "Form compiled in a context requiring no value.")
		 (car successors))
	       ;; We allocate a temporary variable to receive the
	       ;; result, and that variable will not be used.
	       (call-next-method ast
				 (context (list (make-temp nil))
					  successors
					  (invocation context))))
	   ;; We have at least one result.  In case there is more than
	   ;; one, we generate a successor where all but the first one
	   ;; are filled with NIL. 
	   (let ((successor (nil-fill (cdr results) (car successors))))
	     (call-next-method ast
			       (context (list (car results))
					(list successor)
					(invocation context))))))
      (2
       ;; We have a context where a test of a Boolean is required.  We
       ;; create a new context where the result is compared to NIL
       ;; using EQ-INSTRUCTION, and compile the AST in that context
       ;; instead. 
       (let* ((false (cleavir-ir:make-constant-input NIL))
	      (temp (cleavir-ir:new-temporary))
	      (successor (make-instance 'cleavir-ir:eq-instruction
			   :inputs (list temp false)
			   :outputs '()
			   :successors (reverse successors))))
	 (call-next-method ast
			   (context (list temp)
				    (list successor)
				    (invocation context))))))))

(defun check-context-for-one-value-ast (context)
  (assert (and (= (length (results context)) 1)
	       (= (length (successors context)) 1))))

(defun check-context-for-no-value-ast (context)
  (assert (and (zerop (length (results context)))
	       (= (length (successors context)) 1))))

;;; This function is used by compile methods that need a single
;;; successor and that produce a single result.  It takes an arbitrary
;;; context as an argument and returns two values, the successor and a
;;; location for the result. 
(defun adapt-context-1-1 (context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (ecase (length successors)
      (1
       (if (null results)
	   (values (car successors)
		   (make-temp nil))
	   (values (nil-fill (cdr results) (car successors))
		   (car results))))
      (2
       (let ((location (if (null results) (make-temp nil) (car results))))
	 (values (cleavir-ir:make-eq-instruction
		  (list location (cleavir-ir:make-constant-input 'nil))
		  successors)
		 location))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile ASTs that represent Common Lisp operations. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile an IF-AST.  
;;;
;;; We compile the test of the IF-AST in a context where no value is
;;; required and with two successors, the else branch and the then
;;; branch.  The two branches are compiled in the same context as the
;;; IF-AST itself.

(defmethod compile-ast ((ast cleavir-ast:if-ast) context)
  (let ((then-branch (compile-ast (cleavir-ast:then-ast ast) context))
	(else-branch (compile-ast (cleavir-ast:else-ast ast) context)))
    (compile-ast (cleavir-ast:test-ast ast)
		 (context '()
			  (list then-branch else-branch)
			  (invocation context)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a PROGN-AST.
;;;
;;; The last sub-ast is compiled in the same context as the progn-ast
;;; itself.  All the others are copiled in a context where no value is
;;; required, and with the code for the following form as a single
;;; successor.

(defmethod compile-ast ((ast cleavir-ast:progn-ast) context)
  (let ((next (compile-ast (car (last (cleavir-ast:form-asts ast))) context)))
    (loop for sub-ast in (cdr (reverse (cleavir-ast:form-asts ast)))
	  do (setf next (compile-ast sub-ast
				     (context '()
					      (list next)
					      (invocation context)))))
    next))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a BLOCK-AST.
;;;
;;; A BLOCK-AST is compiled by compiling its body in the same context
;;; as the block-ast itself.  However, we store that context in the
;;; *BLOCK-INFO* hash table using the block-ast as a key, so that a
;;; RETURN-FROM-AST that refers to this block can be compiled in the
;;; same context.

(defparameter *block-info* nil)

(defun block-info (block-ast)
  (gethash block-ast *block-info*))

(defun (setf block-info) (new-info block-ast)
  (setf (gethash block-ast *block-info*) new-info))

(defmethod compile-ast ((ast cleavir-ast:block-ast) context)
  (setf (gethash ast *block-info*) context)
  (compile-ast (cleavir-ast:body-ast ast) context))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a RETURN-FROM-AST.
;;;
;;; We must determine whether this RETURN-FROM represents a local
;;; control transfer or a non-local control transfer.  To determine
;;; that, we compare the INVOCATION of the context in which the
;;; corresponding BLOCK-AST was compiled to the INVOCATION of the
;;; current CONTEXT.  If they are the same, we have a local control
;;; transfer.  If not, we have a non-local control transfer.
;;;
;;; If we have a local control transfer, we compile the
;;; RETURN-FROM-AST is as follows: The context is ignored, because the
;;; RETURN-FROM does not return a value in its own context.  Instead,
;;; the FORM-AST of the RETURN-FROM-AST is compiled in the same
;;; context as the corresponding BLOCK-AST was compiled in.
;;;
;;; If we have a non-local control transfer, things might get messy In
;;; the most general case, the corresponding BLOCK-AST was compiled in
;;; a context where all values are needed, AND we can not determine
;;; how many values are returned from the FORM-AST of this
;;; RETURN-FROM-AST.  Then, we must compile the FORM-AST in a context
;;; where we save all the return values in some temporary place.
;;; Following that, we must unwind the stack by executing an
;;; UNWIND-INSTRUCTION.  Finally we must return the saved return
;;; values from that invocation.  We must probably let each
;;; implementation customize how this is done.
;;;
;;; There are some important special cases though.  For instance, if
;;; the BLOCK-AST was compiled in a context where a fixed number of
;;; values are required, then we can compile the FORM-AST of this
;;; RETURN-FROM-AST in a context where the same number of values are
;;; required, save those values in lexical variables, unwind the
;;; stack, and then copy the saved values into the lexical variables
;;; of the target context.  
;;;
;;; The other important special case is when the BLOCK-AST was
;;; compiled in a context where all values are required, but we can
;;; determine how many values are returned by the FORM-AST.  We can
;;; then again save those values in lexical variables, unwind the
;;; stack, and then execute a RETURN-INSTRUCTION with those values as
;;; inputs.  We can determine the number of values returned by the
;;; FORM-AST if it is either a CONSTANT-AST, a LEXICAL-AST, or a
;;; CALL-AST where the function called is VALUES.
;;;
;;; It might be good to apply some transformations to the AST before
;;; it is compiled.  For instance (return-from b (progn f1 f2 f3))
;;; could be transformed into (progn f1 f2 (return-from b f3)) so as
;;; to expose the possibility of f3 to be covered by a special case. 

(defmethod compile-ast ((ast cleavir-ast:return-from-ast) context)
  (let ((block-context (block-info (cleavir-ast:block-ast ast))))
    (if (eq (invocation context)
	    (invocation block-context))
	(compile-ast (cleavir-ast:form-ast ast) block-context)
	(error "can't handle non-local RETURN-FROM yet."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a TAGBODY-AST.
;;;
;;; A TAGBODY-AST is compiled as follows: 
;;;
;;; A single successor is determined as follows:
;;;
;;;   * If the context has no successors, then a RETURN-INSTRUCTION
;;;     which returns a single NIL value is generated, and that
;;;     RETURN-INSTRUCTION becomes the successor.
;;; 
;;;   * If the context has one or more successors and RESULTS is an
;;;     empty list, the successor will be the first in the list.  The
;;;     reason that this method always works, is that if there are
;;;     several successors, then the first one should be chosen when
;;;     the AST yields NIL, which is always the case for the
;;;     TAGBODY-AST.
;;; 
;;;   * If the context has one or more successors and RESULTS contains
;;;     at least one element, then the successor is the first
;;;     instruction in a sequence of instructions that fill the
;;;     results with NIL.

;;; During AST-to-HIR translation, this variable contains a hash table
;;; that maps a TAG-AST to information about the tag.  The information
;;; is a list of two elements.  The first element is the NOP
;;; instruction that will be the target of any GO instruction to that
;;; tag.  The second element is the INVOCATION of the compilation
;;; context, which is used to determine whether the GO to this tag is
;;; local or non-local.
(defparameter *go-info* nil)

(defun go-info (tag-ast)
  (gethash tag-ast *go-info*))

(defun (setf go-info) (new-info tag-ast)
  (setf (gethash tag-ast *go-info*) new-info))

;;; For each TAG-AST in the tagbody, a NOP instruction is created.  A
;;; list containing this instruction and the INVOCATION of the current
;;; context is created.  That list is entered into the hash table
;;; *GO-INFO* using the TAG-AST as a key.  Then the items are compiled
;;; in the reverse order, stacking new instructions before the
;;; successor computed previously.  Compiling a TAG-AST results in the
;;; successor of the corresponding NOP instruction being modified to
;;; point to the remining instructions already computed.  Compiling
;;; something else is done in a context with an empty list of results,
;;; using the remaining instructions already computed as a single
;;; successor.
(defmethod compile-ast ((ast cleavir-ast:tagbody-ast) context)
  (loop for item-ast in (cleavir-ast:item-asts ast)
	do (when (typep item-ast 'cleavir-ast:tag-ast)
	     (setf (go-info item-ast)
		   (list (cleavir-ir:make-nop-instruction nil)
			 (invocation context)))))
  (with-accessors ((results results)
		   (successors successors))
      context
    (let ((next (cond ((null successors)
		       (cleavir-ir:make-return-instruction
			(list (cleavir-ir:make-constant-input 'nil))))
		      ((null results)
		       (car successors))
		      (t
		       (nil-fill results (car successors))))))
      (loop for item-ast in (reverse (cleavir-ast:item-asts ast))
	    do (setf next
		     (if (typep item-ast 'cleavir-ast:tag-ast)
			 (let ((instruction (first (go-info item-ast))))
			   (setf (cleavir-ir:successors instruction)
				 (list next))
			   instruction)
			 (compile-ast item-ast
				      (context '()
					       (list next)
					       (invocation context))))))
      next)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a GO-AST.
;;;
;;; We obtain the GO-INFO that was stored when the TAG-AST of this
;;; GO-AST was compiled.  This info is a list of two elements.  The
;;; first element is the NOP instruction that resulted from the
;;; compilation of the TAG-AST.  The second element is the INVOCATION
;;; of the compilation context when the TAG-AST was compiled.
;;;
;;; The INVOCATION of the parameter CONTEXT is compared to the
;;; invocation in which the target TAG-AST of this GO-AST was
;;; compiled, which is the second element of the GO-INFO list.  If
;;; they are the same, we have a local transfer of control, so we just
;;; return the NOP instruction that resulted from the compilation of
;;; the TAG-AST.  If they are not the same, we generate an
;;; UNWIND-INSTRUCTION with that NOP instruction as its successor, and
;;; we store the INVOCATION of the compilation context when the
;;; TAG-AST was compiled in the UNWIND-INSTRUCTION so that we know how
;;; far to unwind.

(defmethod compile-ast ((ast cleavir-ast:go-ast) context)
  (let ((info (go-info (cleavir-ast:tag-ast ast))))
    (if (eq (second info) (invocation context))
	(first info)
	(cleavir-ir:make-unwind-instruction (first info) (second info)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a CALL-AST.
;;;
;;; If the CALL-AST is compiled in a context with no successors, then
;;; generate a TAILCALL-INSTRUCTION.
;;;
;;; If there is at least one successor, then we must put the values
;;; generated by the call into the syntactic location indicated by the
;;; RESULTS.  This is done by the GET-VALUES-INSTRUCTION.  That
;;; instruction may use one or two successors.  If it has two
;;; successors, tests the first value received and selects a successor
;;; based on whether that value is NIL or something else.

(defmethod compile-ast ((ast cleavir-ast:call-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (let* ((all-args (cons (cleavir-ast:callee-ast ast)
			   (cleavir-ast:argument-asts ast)))
	   (temps (make-temps all-args)))
      (compile-arguments
       all-args
       temps
       (ecase (length successors)
	 (1 (make-instance 'cleavir-ir:funcall-instruction
	      :inputs temps
	      :outputs results
	      :successors successors))
	 (2 (let* ((temp (cleavir-ir:new-temporary))
		   (false (cleavir-ir:make-constant-input nil))
		   (successor (make-instance 'cleavir-ir:eq-instruction
				:inputs (list temp false)
				:outputs '()
				:successors (reverse successors))))
	      (make-instance 'cleavir-ir:funcall-instruction
		:inputs temps
		:outputs (list temp)
		:successors (list successor)))))
       (invocation context)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FUNCTION-AST.
;;;
;;; The FUNCTION-AST represents a closure, so we compile it by
;;; compiling its LAMBDA-LIST and BODY-AST into some code, represented
;;; by the first instruction in the body.  We then generate an
;;; ENCLOSE-INSTRUCTION that takes this code as input.

(defun translate-lambda-list (lambda-list)
  (loop for item in lambda-list
	collect (cond ((member item lambda-list-keywords)
		       item)
		      ((consp item)
		       (if (= (length item) 3)
			   (list (first item)
				 (find-or-create-location (second item))
				 (find-or-create-location (third item)))
			   (list (find-or-create-location (second item))
				 (find-or-create-location (third item)))))
		      (t
		       (find-or-create-location item)))))


;;; The logic of this method is a bit twisted.  The reason is that we
;;; must create the ENTER-INSTRUCTION before we compile the body of
;;; the FUNCTION-AST.  The reason for that is that the
;;; ENTER-INSTRUCTION should be the value of the INVOCATION of the
;;; context when the body is compiled.  On the other hand, the result
;;; of compiling the body must be the successor of the ENTER-INSTRUCTION.  
;;;
;;; We solve this problem by creating the ENTER-INSTRUCTION with a
;;; dummy successor.  Once the body has been compiled, we call
;;; REINITIALIZE-INSTANCE on the ENTER-INSTRUCTION to set the slots to
;;; their final values. 
(defmethod compile-ast ((ast cleavir-ast:function-ast) context)
  (check-context-for-one-value-ast context)
  (let* ((ll (translate-lambda-list (cleavir-ast:lambda-list ast)))
	 (enter (cleavir-ir:make-enter-instruction ll))
	 (body-context (context '() '() enter))
	 (body (compile-ast (cleavir-ast:body-ast ast) body-context)))
    (reinitialize-instance enter :successors (list body))
    (cleavir-ir:make-enclose-instruction
     (first (results context))
     (first (successors context))
     enter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a SETQ-AST.

(defmethod compile-ast ((ast cleavir-ast:setq-ast) context)
  (check-context-for-no-value-ast context)
  (let ((location (find-or-create-location (cleavir-ast:lhs-ast ast))))
    (compile-ast
     (cleavir-ast:value-ast ast)
     (context
      (list location)
      (successors context)
      (invocation context)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a THE-AST.

(defun make-type-check (type-specifier var successor)
  (let* ((type-input (cleavir-ir:make-constant-input type-specifier))
	 (temp (make-temp nil))
	 (error-branch
	   (make-instance 'cleavir-ir:fdefinition-instruction
	     :inputs (list (cleavir-ir:make-constant-input 'error))
	     :outputs (list temp)
	     :successors
	     (list (cleavir-ir:make-funcall-instruction
		    (list temp
			  (cleavir-ir:make-constant-input :datum)
			  var
			  (cleavir-ir:make-constant-input :expected-type)
			  type-input)
		    '())))))
    (cleavir-ir:make-typeq-instruction
     var
     (list error-branch successor)
     type-input)))

(defmethod compile-ast ((ast cleavir-ast:the-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (let ((form-ast (cleavir-ast:form-ast ast))
	  (type-specifiers (cleavir-ast:type-specifiers ast)))
      (ecase (length successors)
	(1
	 (let* ((temp-count (max (length results) (length type-specifiers)))
		(temps (make-temps (make-list temp-count))))
	   (let ((next (car successors)))
	     ;; The last actions to take are to assign the temps to
	     ;; the results.
	     (loop for result in results
		   for temp in temps
		   do (setf next
			    (cleavir-ir:make-assignment-instruction
			     temp result next)))
	     ;; Before assigning to the results, check the
	     ;; types of the values.
	     (loop for type-specifier in type-specifiers
		   for temp in temps
		   do (setf next (make-type-check type-specifier temp next)))
	     next)))
	(2
	 (if (null results)
	     (let ((temp (make-temp nil)))
	       (compile-ast
		ast
		(context
		 (list temp)
		 (list (cleavir-ir:make-eq-instruction
			(list temp (cleavir-ir:make-constant-input 'nil))
			successors))
		 (invocation context))))
	     (compile-ast
	      ast
	      (context
	       (list (car results))
	       (list (nil-fill
		      (cdr results)
		      (cleavir-ir:make-eq-instruction
		       (list (car results)
			     (cleavir-ir:make-constant-input 'nil))
		       successors)))
	       (invocation context)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a SYMBOL-VALUE-AST.

(defmethod compile-ast ((ast cleavir-ast:symbol-value-ast) context)
  (check-context-for-one-value-ast context)
  (let ((temp (make-temp nil)))
    (compile-ast
     (cleavir-ast:symbol-ast ast)
     (context 
      (list temp) 
      (list (make-instance 'cleavir-ir:symbol-value-instruction
	      :inputs (list temp)
	      :outputs (results context)
	      :successors (successors context)))
      (invocation context)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a SET-SYMBOL-VALUE-AST.

(defmethod compile-ast ((ast cleavir-ast:set-symbol-value-ast) context)
  (check-context-for-no-value-ast context)
  (let ((temp1 (make-temp nil))
	(temp2 (make-temp nil)))
    (compile-ast
     (cleavir-ast:symbol-ast ast)
     (context 
      (list temp1) 
      (list (compile-ast
	     (cleavir-ast:value-ast ast)
	     (context
	      (list temp2)
	      (list (make-instance 'cleavir-ir:set-symbol-value-instruction
		      :inputs (list temp1 temp2)
		      :outputs ()
		      :successors (successors context)))
	      (invocation context))))
      (invocation context)))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FDEFINITION-AST.

(defmethod compile-ast ((ast cleavir-ast:fdefinition-ast) context)
  (check-context-for-one-value-ast context)
  (let ((temp (make-temp nil)))
    (compile-ast
     (cleavir-ast:name-ast ast)
     (context 
      (list temp) 
      (list (make-instance 'cleavir-ir:fdefinition-instruction
	      :inputs (list temp)
	      :outputs (results context)
	      :successors (successors context)))
      (invocation context)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a TYPEQ-AST.

(defun make-boolean (boolean result successor)
  (cleavir-ir:make-assignment-instruction
   (cleavir-ir:make-constant-input boolean)
   result
   successor))

(defmethod compile-ast ((ast cleavir-ast:typeq-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (ecase (length successors)
      (1 (if (null results)
	     (progn (warn "test compiled in a context with no results")
		    (car successors))
	     (let* ((false (make-boolean nil (car results) (car successors)))
		    (true (make-boolean t (car results) (car successors)))
		    (temp (make-temp nil)))
	       (compile-ast
		(cleavir-ast:form-ast ast)
		(context
		 (list temp)
		 (list
		  (nil-fill
		   (cdr results)
		   (cleavir-ir:make-typeq-instruction
		    temp
		    (list false true)
		    (cleavir-ast:type-specifier ast))))
		 (invocation context))))))
      (2 (if (null results)
	     (let ((temp (make-temp nil)))
	       (compile-ast
		(cleavir-ast:form-ast ast)
		(context
		 (list temp)
		 (list (cleavir-ir:make-typeq-instruction
			temp
			successors
			(cleavir-ast:type-specifier ast)))
		 (invocation context))))
	     (let ((false (make-boolean nil (car results) (car successors)))
		   (true (make-boolean t (car results) (cadr successors)))
		   (temp (make-temp nil)))
	       (compile-ast
		(cleavir-ast:form-ast ast)
		(context
		 (list temp)
		 (list
		  (nil-fill
		   (cdr results)
		   (cleavir-ir:make-typeq-instruction
		    temp
		    (list false true)
		    (cleavir-ast:type-specifier ast))))
		 (invocation context)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a LEXICAL-AST.
;;;
;;; This AST has ONE-VALUE-AST-MIXIN as a superclass. 

(defmethod compile-ast ((ast cleavir-ast:lexical-ast) context)
  (check-context-for-one-value-ast context)
  (cleavir-ir:make-assignment-instruction
   (find-or-create-location ast)
   (first (results context))
   (first (successors context))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COMPILE-TOPLEVEL

(defun compile-toplevel (ast)
  (let ((*block-info* (make-hash-table :test #'eq))
	(*go-info* (make-hash-table :test #'eq))
	(*location-info* (make-hash-table :test #'eq)))
    (compile-ast ast (context '() '() nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile ASTs that represent low-level operations.

(defun make-temps (arguments)
  (loop for argument in arguments
	collect (make-temp argument)))

(defun compile-arguments (arguments temps successor invocation)
  (loop with succ = successor
	for arg in (reverse arguments)
	for temp in (reverse temps)
	do (setf succ (compile-ast arg
				   (context `(,temp)
					    `(,succ)
					    invocation)))
	finally (return succ)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a CONSTANT-AST.
;;;

(defmethod compile-ast ((ast cleavir-ast:constant-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (ecase (length successors)
      (1 (if (null results)
	     (progn 
	       (warn "constant compiled in a context with no values")
	       (car successors))
	     (cleavir-ir:make-assignment-instruction
	      (cleavir-ir:make-constant-input (cleavir-ast:value ast))
	      (car results)
	      (nil-fill (cdr results) (car successors)))))
      (2 (if (null results)
	     (if (null (cleavir-ast:value ast))
		 (car successors)
		 (cadr successors))
	     (cleavir-ir:make-assignment-instruction
	      (cleavir-ir:make-constant-input (cleavir-ast:value ast))
	      (car results)
	      (if (null (cleavir-ast:value ast))
		  (car successors)
		  (cadr successors))))))))
