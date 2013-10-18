(in-package #:sicl-compiler-phase-2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compilation context.
;;;
;;; Each AST is compiled in a particular COMPILATION CONTEXT or
;;; CONTEXT for short.  A context object has three components: 
;;;
;;; 1. SUCCESSORS, which is a proper list containing zero, one or two
;;; elements.  These elements are instructions resulting from the
;;; generation of the code that should be executed AFTER the code
;;; generated from this AST.  If the list contains two elements, then
;;; this AST is compiled in a context where a Boolean result is
;;; required.  In this case, the first element of the list is the
;;; successor to use when the value generated by the AST is NIL, and
;;; the second element is the successor to use when the value
;;; generated by the AST is something other than NIL.  If there are no
;;; successors, as indicated by SUCCESSORS being the empty list, then
;;; either a TAILCALL-INSTRUCTION (if the AST is a CALL-AST) or a
;;; RETURN-INSTRUCTION (for other ASTs) that returns all the values of
;;; the AST should be generated.
;;;
;;; 2. RESULTS, a proper list indicating how many values are required
;;; from the compilation of this AST.  It contains a list of lexical
;;; locations into which the generated code must put the values of
;;; this AST.  If the list is empty, it means either that no values
;;; are required (when SUCCESSORS > 0) or that ALL values are requried
;;; (when SUCCESSORS = 0).  If the list contains more elements than
;;; the number of values generated by this AST, then the remaining
;;; lexical locations in the list must be filled with NIL by the code
;;; generated from this AST.
;;;
;;; 3. FALSE-REQUIRED-P, which is a Boolean value indicating whether a
;;; NIL Boolean value is required as explained below.
;;;
;;; The following combinations can occur:
;;;
;;;  * SUCCESSORS and RESULTS are both empty lists.  FALSE-REQUIRED-P
;;;    is ignored.  In this case, either a TAILCALL-INSTRUCTION (if
;;;    the AST is a CALL-AST) or a RETURN-INSTRUCTION (for other ASTs)
;;;    that returns all the values of the AST should be generated.
;;;
;;;  * There is a single successor.  Then RESULTS can contain any
;;;    number of elements.  FALSE-REQUIRED-P is ignored.  Code should
;;;    be generated to assign values to the lexical locations in
;;;    RESULTS.  If there are fewer such locations than the number of
;;;    values that the AST would normally produce, then special
;;;    (optimized) code could be generated if possible.  If there are
;;;    more locations than the number of values the AST would normally
;;;    produce, code must be generated to fill the additional
;;;    locations with NIL.
;;;
;;;  * There are two successors and the RESULTS is the empty list.
;;;    Then the generated code should determine whether the AST
;;;    generates a false or a true value and select the appropriate
;;;    successor.  FALSE-REQUIRED-P is ignored.  Such a context is
;;;    used to compile the test of an IF form.  The two successors
;;;    then correspond to the code for the ELSE branch and the code
;;;    for the THEN branch respectively (in that order!).
;;;
;;;  * There are two successors and the RESULTS is a list with more
;;;    than one element.  FALSE-REQUIRED-P is ignored.  The code
;;;    generated from the AST should do two things.  Code should be
;;;    generated to assign values to the results, and according to
;;;    whether the FIRST value is false or true, the appropriate
;;;    successor should be selected.  This kind of context could be
;;;    used to compile the FORM in (if (setf (values x y) FORM) ...).
;;;
;;;  * There are two successors and the RESULTS is a list with exactly
;;;    one element.  FALSE-REQUIRED-P is true.  The code generated
;;;    from the AST should do two things.  First, it should generate
;;;    code to compute the value from the AST and store it in the
;;;    result.  Next, it should determine whether that value is false
;;;    or true, and select the appropriate successor.  This kind of
;;;    context could be used to compile the FORM in code such as 
;;;    (if (setq x FORM) ...)
;;;
;;;  * There are two successors and the RESULTS is a list with exactly
;;;    one element.  FALSE-REQUIRED-P is false.  The code generated
;;;    should determine whether the result is false or true.  If it is
;;;    false, the first successor should be selected.  If it is true,
;;;    then that true value should be assigned to the lexical location
;;;    in RESULTS and the second successor should be selected.  This
;;;    kind of context could be used to compile FORM in code such as
;;;    (setq x (or FORM ...)).  However, it is never an error to
;;;    supply the false value even when it is not required.

(defclass context ()
  ((%results :initarg :results :reader results)
   (%successors :initarg :successors :accessor successors)
   (%false-required-p :initarg :false-required-p :reader false-required-p)))

(defun context (results successors &optional (false-required-p nil))
  (unless (and (listp results)
	       (every (lambda (result)
			(typep result 'sicl-mir:lexical-location))
		      results))
    (error "illegal results: ~s" results))
  (unless (and (listp successors)
	       (every (lambda (successor)
			(typep successor 'sicl-mir:instruction))
		      successors))
    (error "illegal successors: ~s" results))
  (when (and (null successors) (not (null results)))
    (error "Illegal combination of results and successors"))
  (make-instance 'context
    :results results
    :successors successors
    :false-required-p false-required-p))

(defmethod print-object ((obj context) stream)
  (print-unreadable-object (obj stream)
    (format stream " results: ~s" (results obj))
    (format stream " successors: ~s" (successors obj))
    (format stream " false-required-p: ~s" (false-required-p obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; During compilation, this variable contains a hash table that maps
;;; ASTs representing locations to MIR locations.

(defparameter *location-info* nil)

(defun find-or-create-location (ast)
  (or (gethash ast *location-info*)
      (let ((location
	      (etypecase ast
		(sicl-ast:lexical-ast
		 (sicl-mir:make-lexical-location
		  (sicl-ast:name ast)))
		(sicl-ast:global-ast
		 (sicl-mir:make-global-input
		  (sicl-ast:name ast)
		  (sicl-ast:storage ast)))
		(sicl-ast:special-ast
		 (sicl-mir:make-special-location
		  (sicl-ast:name ast)
		  (sicl-ast:storage ast))))))
	(setf (gethash ast *location-info*) location))))

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
		   (sicl-mir:make-assignment-instruction
		    (sicl-mir:make-constant-input 'nil)
		    value next))
	  finally (return next))))

(defgeneric compile-ast (ast context))

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

(defmethod compile-ast ((ast sicl-ast:if-ast) context)
  (let ((then-branch (compile-ast (sicl-ast:then-ast ast) context))
	(else-branch (compile-ast (sicl-ast:else-ast ast) context)))
    (compile-ast (sicl-ast:test-ast ast)
		 (context '() (list else-branch then-branch)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a PROGN-AST.
;;;
;;; The last sub-ast is compiled in the same context as the progn-ast
;;; itself.  All the others are copiled in a context where no value is
;;; required, and with the code for the following form as a single
;;; successor.

(defmethod compile-ast ((ast sicl-ast:progn-ast) context)
  (let ((next (compile-ast (car (last (sicl-ast:form-asts ast))) context)))
    (loop for sub-ast in (cdr (reverse (sicl-ast:form-asts ast)))
	  do (setf next (compile-ast sub-ast (context '() (list next)))))
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

(defmethod compile-ast ((ast sicl-ast:block-ast) context)
  (setf (gethash ast *block-info*) context)
  (compile-ast (sicl-ast:body-ast ast) context))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a RETURN-FROM-AST.
;;;
;;; A RETURN-FROM-AST is compiled as follows: The context is ignored,
;;; because the RETURN-FROM does not return a value in its own
;;; context.  Instead, the FORM-AST of the RETURN-FROM-AST is compiled
;;; in the same context as the corresponding BLOCK-AST was compiled
;;; in.

(defmethod compile-ast ((ast sicl-ast:return-from-ast) context)
  (declare (ignore context))
  (let ((block-context (gethash (sicl-ast:block-ast ast) *block-info*)))
    (compile-ast (sicl-ast:form-ast ast) block-context)))

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
;;; 
;;; For each TAG-AST in the tagbody, a NOP instruction is created and
;;; that instruction is entered into the hash table *GO-INFO* using
;;; the TAG-AST as a key.  Then the items are compiled in the reverse
;;; order, stacking new instructions before the successor computed
;;; previously.  Compiling a TAG-AST results in the successor of the
;;; corresponding NOP instruction being modified to point to the
;;; remining instructions already computed.  Compiling something else
;;; is done in a context with an empty list of results, using the
;;; remaining instructions already computed as a single successor.

(defparameter *go-info* nil)

(defmethod compile-ast ((ast sicl-ast:tagbody-ast) context)
  (loop for item in (sicl-ast:items ast)
	do (when (typep item 'sicl-ast:tag-ast)
	     (setf (gethash item *go-info*)
		   (sicl-mir:make-nop-instruction nil))))
  (with-accessors ((results results)
		   (successors successors))
      context
    (let ((next (cond ((null successors)
		       (sicl-mir:make-return-instruction
			(list (sicl-mir:make-constant-input 'nil))))
		      ((null results)
		       (car successors))
		      (t
		       (nil-fill results (car successors))))))
      (loop for item in (reverse (sicl-ast:items ast))
	    do (setf next
		     (if (typep item 'sicl-ast:tag-ast)
			 (let ((instruction (gethash item *go-info*)))
			   (setf (sicl-mir:successors instruction)
				 (list next))
			   instruction)
			 (compile-ast item (context '() (list next))))))
      next)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a GO-AST.
;;;
;;; The CONTEXT is ignored.  Instead, the successor becomes the NOP
;;; instruction that was entered into the hash table *GO-INFO* when
;;; the TAGBODY-AST was compiled.

(defmethod compile-ast ((ast sicl-ast:go-ast) context)
  (declare (ignore context))
  (gethash (sicl-ast:tag-ast ast) *go-info*))

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

(defmethod compile-ast ((ast sicl-ast:call-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (let* ((all-args (cons (sicl-ast:callee-ast ast)
			   (sicl-ast:argument-asts ast)))
	   (temps (make-temps all-args)))
      (compile-arguments
       all-args
       temps
       ;; This is a temporary kludge to take advantage of the fact
       ;; that the function ERROR never returns.  A more systematic
       ;; approach would be to check that the function is system
       ;; supplied and that its return type is NIL.
       (if (and (typep (sicl-ast:callee-ast ast) 'sicl-ast:global-ast)
		(eq (sicl-ast:name (sicl-ast:callee-ast ast)) 'error))
	   (sicl-mir:make-funcall-instruction temps)
	   (ecase (length successors)
	     (0 (sicl-mir:make-tailcall-instruction temps))
	     (1 (sicl-mir:make-funcall-instruction
		 temps
		 (sicl-mir:make-get-values-instruction
		  results (car successors))))
	     (2 (let ((temp (sicl-mir:new-temporary)))
		  (sicl-mir:make-funcall-instruction
		   temps
		   (sicl-mir:make-get-values-instruction
		    (list temp)
		    (sicl-mir:make-==-instruction
		     (list temp (sicl-mir:make-constant-input nil))
		     successors)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a function.

(defun compile-function (body-ast)
  (sicl-mir:make-enter-instruction 
   (compile-ast body-ast (context '() '()))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FUNCTION-AST.
;;;
;;; The FUNCTION-AST represents a closure, so we compile it by
;;; compiling its LAMBDA-LIST and BODY-AST into some code, represented
;;; by the first instruction in the body.  We then generate an
;;; ENCLOSE-INSTRUCTION that takes this code as input.
;;;
;;; The value computed by the FUNCTION-AST is always a function, so it
;;; is always a single non-NIL value.  If there are no successors, we
;;; also generate a RETURN-INSTRUCTION with the single value as input.
;;; If there is more than one successor, chose the second one for the
;;; true value.

(defmethod compile-ast ((ast sicl-ast:function-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (let ((code (compile-function (sicl-ast:body-ast ast))))
      (ecase (length successors)
	(0 (let ((temp (sicl-mir:new-temporary)))
	     (sicl-mir:make-enclose-instruction
	      temp
	      (sicl-mir:make-return-instruction (list temp))
	      code)))
	(1 (if (null results)
	       (progn (warn "closure compiled in a context with no values")
		      (car successors))
	       (sicl-mir:make-enclose-instruction
		(car results)
		(nil-fill (cdr results) (car successors))
		code)))
	(2 (if (null results)
	       (car successors)
	       (sicl-mir:make-enclose-instruction
		(car results)
		(nil-fill (cdr results) (cadr successors))
		code)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a SETQ-AST.

(defmethod compile-ast ((ast sicl-ast:setq-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (let* ((location (find-or-create-location (sicl-ast:lhs-ast ast)))
	   (next 
	     (ecase (length successors)
	       (0 (sicl-mir:make-return-instruction (list location)))
	       (1 (if (null results)
		      (car successors)
		      (sicl-mir:make-assignment-instruction
		       location
		       (car results)
		       (nil-fill (cdr results) (car successors)))))
	       (2 (if (null results)
		      (sicl-mir:make-==-instruction
		       (list location (sicl-mir:make-constant-input 'nil))
		       successors)
		      (sicl-mir:make-assignment-instruction
		       location
		       (car results)
		       (sicl-mir:make-==-instruction
			(list location (sicl-mir:make-constant-input 'nil))
			successors)))))))
      (compile-ast (sicl-ast:value-ast ast)
		   (context (list location) (list next))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a THE-AST.

(defun make-type-check (type-ast var successor)
  (check-type type-ast sicl-ast:constant-ast)
  (let* ((type-input (sicl-mir:make-constant-input (sicl-ast:value type-ast)))
	 (error-branch
	   (sicl-mir:make-funcall-instruction
	    ;; We do not know the storage for the ERROR function, but
	    ;; it should already have been processed so that it is
	    ;; present in the linkage vector.
	    (list (sicl-mir:make-global-input 'error nil)
		  (sicl-mir:make-constant-input :datum)
		  var
		  (sicl-mir:make-constant-input :expected-type)
		  type-input))))
    (sicl-mir:make-typeq-instruction
     (list var type-input)
     (list error-branch successor))))

(defmethod compile-ast ((ast sicl-ast:the-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (destructuring-bind (form-ast . type-asts)
	(sicl-ast:children ast)
      (ecase (length successors)
	(0
	 ;; This case is a bit hard to handle, because we don't know a
	 ;; priori how many values are returned by the FORM-AST if the
	 ;; THE-AST.  For now, do what we are allowed to do according to
	 ;; the HyperSpec (i.e., don't check the types).  Eventually we
	 ;; hope to implement a better solution.
	 (compile-ast form-ast context))
	(1
	 (let* ((temp-count (max (length results) (length type-asts)))
		(temps (make-temps (make-list temp-count))))
	   (let ((next (car successors)))
	     ;; The last actions to take are to assign the temps to
	     ;; the results.
	     (loop for result in results
		   for temp in temps
		   do (setf next
			    (sicl-mir:make-assignment-instruction
			     temp result next)))
	     ;; Before assigning to the results, check the
	     ;; types of the values.
	     (loop for type-ast in type-asts
		   for temp in temps
		   do (setf next (make-type-check type-ast temp next)))
	     next)))
	(2
	 (if (null results)
	     (let ((temp (make-temp nil)))
	       (compile-ast
		ast
		(context
		 (list temp)
		 (list (sicl-mir:make-==-instruction
			(list temp (sicl-mir:make-constant-input 'nil))
			successors)))))
	     (compile-ast
	      ast
	      (context
	       (list (car results))
	       (list (nil-fill
		      (cdr results)
		      (sicl-mir:make-==-instruction
		       (list (car results)
			     (sicl-mir:make-constant-input 'nil))
		       successors)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a TYPEQ-AST.

(defmethod compile-ast ((ast sicl-ast:typeq-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (ecase (length successors)
      (0 
       (let* ((temp1 (make-temp nil))
	      (next (sicl-mir:make-return-instruction (list temp1)))
	      (false (make-boolean nil temp1 next))
	      (true (make-boolean t temp1 next))
	      (temp2 (make-temp nil)))
	 (compile-ast
	  (sicl-ast:form-ast ast)
	  (context
	   (list temp2)
	   (list (sicl-mir:make-typeq-instruction
		  (list temp2
			(sicl-mir:make-constant-input
			 (sicl-ast:type-specifier ast)))
		  (list false true)))))))
      (1 (if (null results)
	     (progn (warn "test compiled in a context with no results")
		    (car successors))
	     (let* ((false (make-boolean nil (car results) (car successors)))
		    (true (make-boolean t (car results) (car successors)))
		    (temp (make-temp nil)))
	       (compile-ast
		(sicl-ast:form-ast ast)
		(context
		 (list temp)
		 (list
		  (nil-fill
		   (cdr results)
		   (sicl-mir:make-typeq-instruction
		    (list temp
			  (sicl-mir:make-constant-input
			   (sicl-ast:type-specifier ast)))
		    (list false true)))))))))
      (2 (if (null results)
	     (let ((temp (make-temp nil)))
	       (compile-ast
		(sicl-ast:form-ast ast)
		(context
		 (list temp)
		 (list (sicl-mir:make-typeq-instruction
			(list temp
			      (sicl-mir:make-constant-input
			       (sicl-ast:type-specifier ast)))
			successors)))))
	     (let ((false (make-boolean nil (car results) (car successors)))
		   (true (make-boolean t (car results) (cadr successors)))
		   (temp (make-temp nil)))
	       (compile-ast
		(sicl-ast:form-ast ast)
		(context
		 (list temp)
		 (list
		  (nil-fill
		   (cdr results)
		   (sicl-mir:make-typeq-instruction
		    (list temp
			  (sicl-mir:make-constant-input
			   (sicl-ast:type-specifier ast)))
		    (list false true))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a LEXICAL-AST.
;;;
;;; If there are no successors, then we generate a RETURN-INSTRUCTION.
;;; If there is a single successor and the RESULTS is the empty list,
;;; then a lexical variable occurs in a context where its value is not
;;; required, so we warn, and generate no additional code.  If there
;;; is a single successor and the RESULTS contains a single element,
;;; we generate an ASSIGNMENT-INSTRUCTION.
;;;
;;; If there are two successors, we must generate a ==-INSTRUCTION
;;; with those two successor.  If in addition the RESULTS is not the
;;; empty list, we must also generate an ASSIGNMENT-INSTRUCTION.

(defmethod compile-ast ((ast sicl-ast:lexical-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (let ((location (find-or-create-location ast)))
      (ecase (length successors)
	(0 (sicl-mir:make-return-instruction
	    (list location)))
	(1 (if (null results)
	       (progn 
		 (warn "variable compiled in a context with no values")
		 (car successors))
	       (sicl-mir:make-assignment-instruction
		location
		(car results) 
		(nil-fill (cdr results) (car successors)))))
	(2 (if (null results)
	       (sicl-mir:make-==-instruction
		(list location (sicl-mir:make-constant-input nil))
		successors)
	       (sicl-mir:make-assignment-instruction
		location
		(car results)
		(nil-fill
		 (cdr results)
		 (sicl-mir:make-==-instruction
		  (list location (sicl-mir:make-constant-input nil))
		  successors)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a GLOBAL-AST.
;;;
;;; If there are no successors, then we generate a RETURN-INSTRUCTION.
;;; If there is a single successor and the RESULTS is the empty list,
;;; then a global reference occurs in a context where its value is not
;;; required, so we warn, and generate no additional code.  If there
;;; is a single successor and the RESULTS contains a single element,
;;; we generate an ASSIGNMENT-INSTRUCTION.
;;;
;;; If there are two successors, we must generate a ==-INSTRUCTION
;;; with those two successor.  If in addition the RESULTS is not the
;;; empty list, we must also generate an ASSIGNMENT-INSTRUCTION.

(defmethod compile-ast ((ast sicl-ast:global-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (let ((location (find-or-create-location ast)))
      (ecase (length successors)
	(0 (sicl-mir:make-return-instruction
	    (list location)))
	(1 (if (null results)
	       (progn 
		 (warn "form compiled in a context with no values")
		 (car successors))
	       (sicl-mir:make-assignment-instruction
		location
		(car results) 
		(nil-fill (cdr results) (car successors)))))
	(2 (if (null results)
	       (sicl-mir:make-==-instruction
		(list location (sicl-mir:make-constant-input nil))
		successors)
	       (sicl-mir:make-assignment-instruction
		location
		(car results)
		(nil-fill
		 (cdr results)
		 (sicl-mir:make-==-instruction
		  (list location (sicl-mir:make-constant-input nil))
		  successors)))))))))

(defun compile-toplevel (ast)
  (let ((*block-info* (make-hash-table :test #'eq))
	(*go-info* (make-hash-table :test #'eq))
	(*location-info* (make-hash-table :test #'eq)))
    ;; The top-level ast must represent a thunk.
    (assert (typep ast 'sicl-ast:function-ast))
    (compile-function (sicl-ast:body-ast ast))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile an ARGCOUNT-AST.
;;;
;;; The ARGCOUNT AST is known only to be found in a context where
;;; there is a single result required, and a single successor.

(defmethod compile-ast ((ast sicl-ast:argcount-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (sicl-mir:make-get-argcount-instruction (car results) (car successors))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile an ARG-AST.
;;;
;;; The ARG AST is known only to be found in a context where
;;; there is a single result required, and a single successor.

(defmethod compile-ast ((ast sicl-ast:arg-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    ;; This is a kludge.  If the INDEX-AST of the AST is a
    ;; CONSTANT-AST, then we make a special case, so that we later can
    ;; easily recognize a GET-ARG instruction with a constant input.
    (if (typep (sicl-ast:index-ast ast) 'sicl-ast:constant-ast)
	(let ((value (sicl-ast:value (sicl-ast:index-ast ast))))
	  (sicl-mir:make-get-arg-instruction
	   (sicl-mir:make-constant-input value)
	   (car results) (car successors)))
	(let ((temp (sicl-mir:new-temporary)))
	  (compile-ast (sicl-ast:index-ast ast)
		       (context
			(list temp)
			(list (sicl-mir:make-get-arg-instruction
			       temp (car results) (car successors)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile ASTs that represent low-level operations.

(defun make-temp (argument)
  (declare (ignore argument))
  (sicl-mir:new-temporary))

(defun make-temps (arguments)
  (loop for argument in arguments
	collect (make-temp argument)))

(defun compile-arguments (arguments temps successor)
  (loop with succ = successor
	for arg in (reverse arguments)
	for temp in (reverse temps)
	do (unless (or (typep temp 'sicl-mir:immediate-input)
		       (typep temp 'sicl-mir:external-input))
	     (setf succ (compile-ast arg (context `(,temp) `(,succ)))))
	finally (return succ)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a WORD-AST.
;;;

(defmethod compile-ast ((ast sicl-ast:word-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (unless (and (= (length results) 1)
		 (= (length successors) 1))
      (error "Invalid results for word."))
    (sicl-mir:make-assignment-instruction
     (sicl-mir:make-word-input (sicl-ast:value ast))
     (car results)
     (car successors))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a CONSTANT-AST.
;;;

(defmethod compile-ast ((ast sicl-ast:constant-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (ecase (length successors)
      (0 (sicl-mir:make-return-instruction
	  (list (sicl-mir:make-constant-input (sicl-ast:value ast)))))
      (1 (if (null results)
	     (progn 
	       (warn "constant compiled in a context with no values")
	       (car successors))
	     (sicl-mir:make-assignment-instruction
	      (sicl-mir:make-constant-input (sicl-ast:value ast))
	      (car results)
	      (nil-fill (cdr results) (car successors)))))
      (2 (if (null results)
	     (if (null (sicl-ast:value ast))
		 (car successors)
		 (cadr successors))
	     (sicl-mir:make-assignment-instruction
	      (sicl-mir:make-constant-input (sicl-ast:value ast))
	      (car results)
	      (if (null (sicl-ast:value ast))
		  (car successors)
		  (cadr successors))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a MEMREF-AST.
;;;

(defmethod compile-ast ((ast sicl-ast:memref-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (let* ((temps (make-temps (sicl-ast:argument-asts ast)))
	   (instruction
	     (ecase (length successors)
	       (1 (let ((next (car successors)))
		    (cond ((null results)
			   (warn "MEMREF operation in a context of no results.")
			   next)
			  ((eq results t)
			   (let ((temp2 (sicl-mir:new-temporary)))
			     (setf next 
				   (sicl-mir:make-return-instruction
				    (list temp2)))
			     (sicl-mir:make-memref-instruction
			      (car temps) temp2 next)))
			  (t
			   (setf next (nil-fill (cdr results) next))
			   (sicl-mir:make-memref-instruction
			    (car temps) (car results) next)))))
	       (2 (if (eq results t)
		      (error "Illegal context for memref")
		      (let* ((location (if (null results)
					   (sicl-mir:new-temporary)
					   (car results)))
			     (next (sicl-mir:make-==-instruction
				    (list location
					  (sicl-mir:make-constant-input nil))
				    successors)))
			(setf next
			      (sicl-mir:make-memref-instruction
			       (car temps) location next))
			(nil-fill (cdr results) next)))))))
      (compile-arguments (sicl-ast:argument-asts ast) temps instruction))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a MEMSET-AST.
;;;

(defmethod compile-ast ((ast sicl-ast:memset-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (unless (and (= (length successors) 1)
		 (zerop (length results)))
      (error "Illegal context for memset."))
    (let* ((temps (make-temps (sicl-ast:argument-asts ast)))
	   (instruction
	     (sicl-mir:make-memset-instruction
	      temps (car successors))))
      (compile-arguments (sicl-ast:argument-asts ast) temps instruction))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiling a simple arithmetic operation.

(defun compile-simple-arithmetic (argument-asts constructor context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (let* ((temps (make-temps argument-asts))
	   (instruction
	     (ecase (length successors)
	       (1 (let ((next (car successors)))
		    (cond ((null results)
			   (warn "Arithmetic operation in a context of no results.")
			   next)
			  ((eq results t)
			   (let ((temp (sicl-mir:new-temporary)))
			     (setf next 
				   (sicl-mir:make-return-instruction
				    (list temp)))
			     (funcall constructor temps temp (list next))))
			  (t
			   (setf next (nil-fill (cdr results) next))
			   (funcall constructor
				    temps (car results) (list next))))))
	       (2 (if (or (eq results t) (> (length results) 1))
		      (error "Illegal context for simple arithmetic.")
		      (funcall constructor temps successors))))))
      (compile-arguments argument-asts temps instruction))))

(defmethod compile-ast ((ast sicl-ast:u+-ast) context)
  (compile-simple-arithmetic (sicl-ast:argument-asts ast)
			     #'sicl-mir:make-u+-instruction
			     context))

(defmethod compile-ast ((ast sicl-ast:u--ast) context)
  (compile-simple-arithmetic (sicl-ast:argument-asts ast)
			     #'sicl-mir:make-u--instruction
			     context))

(defmethod compile-ast ((ast sicl-ast:s+-ast) context)
  (compile-simple-arithmetic (sicl-ast:argument-asts ast)
			     #'sicl-mir:make-u--instruction
			     context))

(defmethod compile-ast ((ast sicl-ast:s--ast) context)
  (compile-simple-arithmetic (sicl-ast:argument-asts ast)
			     #'sicl-mir:make-u--instruction
			     context))

(defmethod compile-ast ((ast sicl-ast:neg-ast) context)
  (compile-simple-arithmetic (sicl-ast:argument-asts ast)
			     #'sicl-mir:make-u--instruction
			     context))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a logic operation.
;;;
;;; Logic operations are characterized by the fact that they compute a
;;; single value and that this value can not generate an overflow or a
;;; carry.  Therefore, the corresponding instruction must have a
;;; single successor.
;;;
;;; We can not exclude that the result of a logic operation is a
;;; tagged Lisp object, so we must be prepared for all possible
;;; result contexts. 

(defun compile-logic (argument-asts constructor context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (unless (= (length successors) 1)
      (error "Logic operation must have a single successor."))
    (let* ((next (car successors))
	   (temps (make-temps argument-asts))
	   (instruction
	     (cond ((null results)
		    (warn "Logic operation in a context of no results.")
		    next)
		   ((eq results t)
		    (let ((temp (sicl-mir:new-temporary)))
		      (setf next 
			    (sicl-mir:make-return-instruction
			     (list temp)))
		      (funcall constructor temps temp next)))
		   (t
		    (setf next (nil-fill (cdr results) next))
		    (funcall constructor temps (car results) next)))))
      (compile-arguments argument-asts temps instruction))))

      
(defmethod compile-ast ((ast sicl-ast:&-ast) context)
  (compile-logic (sicl-ast:argument-asts ast)
		 #'sicl-mir:make-&-instruction
		 context))

(defmethod compile-ast ((ast sicl-ast:ior-ast) context)
  (compile-logic (sicl-ast:argument-asts ast)
		 #'sicl-mir:make-ior-instruction
		 context))

(defmethod compile-ast ((ast sicl-ast:xor-ast) context)
  (compile-logic (sicl-ast:argument-asts ast)
		 #'sicl-mir:make-xor-instruction
		 context))

(defmethod compile-ast ((ast sicl-ast:~-ast) context)
  (compile-logic (sicl-ast:argument-asts ast)
		 #'sicl-mir:make-~-instruction
		 context))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a test.

(defun make-boolean (boolean result successor)
  (sicl-mir:make-assignment-instruction
   (sicl-mir:make-external-input boolean)
   result
   successor))

(defun compile-test (argument-asts constructor context)
  (with-accessors ((results results)
		   (successors successors)
		   (false-required-p false-required-p ))
      context
    (let* ((temps (make-temps argument-asts))
	   (instruction
	     (ecase (length successors)
	       (1 (let ((next (car successors)))
		    (cond ((null results)
			   (warn "Compilation of a test that is not used.")
			   next)
			  ((eq results t)
			   (let ((temp (sicl-mir:new-temporary)))
			     (setf next 
				   (sicl-mir:make-return-instruction
				    (list temp)))
			     (let ((false (make-boolean nil temp next))
				   (true (make-boolean t temp next)))
			       (funcall constructor temps (list false true)))))
			  (t
			   (setf next (nil-fill (cdr results) next))
			   (let ((false (make-boolean nil (car results) next))
				 (true (make-boolean t (car results) next)))
			     (funcall constructor temps (list false true)))))))
	       (2 (if (null results)
		      (funcall constructor temps successors)
		      (let ((next (funcall constructor temps successors)))
			(setf next (nil-fill (cdr results) next))
			(let ((false (if false-required-p
					 (make-boolean nil (car results) next)
					 next))
			      (true (make-boolean t (car results) next)))
			  (funcall constructor temps (list false true)))))))))
      (compile-arguments argument-asts temps instruction))))

(defmethod compile-ast ((ast sicl-ast:==-ast) context)
  (compile-test (sicl-ast:argument-asts ast)
		#'sicl-mir:make-==-instruction
		context))

(defmethod compile-ast ((ast sicl-ast:s<-ast) context)
  (compile-test (sicl-ast:argument-asts ast)
		#'sicl-mir:make-s<-instruction
		context))

(defmethod compile-ast ((ast sicl-ast:s<=-ast) context)
  (compile-test (sicl-ast:argument-asts ast)
		#'sicl-mir:make-s<=-instruction
		context))

(defmethod compile-ast ((ast sicl-ast:u<-ast) context)
  (compile-test (sicl-ast:argument-asts ast)
		#'sicl-mir:make-u<-instruction
		context))

(defmethod compile-ast ((ast sicl-ast:u<=-ast) context)
  (compile-test (sicl-ast:argument-asts ast)
		#'sicl-mir:make-u<=-instruction
		context))

