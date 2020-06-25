(cl:in-package #:cleavir-ast-to-hir)

;;; During compilation, this variable contains a hash table that maps
;;; ASTs representing locations to HIR locations.
(defvar *location-info*)

;;; Given an AST of type LEXICAL-LOCATION, return a corresponding HIR
;;; lexical location.  If no corresponding HIR location is found, one
;;; is created and retured, and made to correspond to the AST in
;;; future invocations.
(defun find-or-create-location (ast)
  (or (gethash ast *location-info*)
      (let ((location
	      (etypecase ast
		(cleavir-ast:lexical-ast
		 (cleavir-ir:make-lexical-location
		  (cleavir-ast:name ast))))))
	(setf (gethash ast *location-info*) location))))

;;; Convenience function to avoid having long function names.
(defun make-temp ()
  (cleavir-ir:new-temporary))

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

;;; The generic function called on various AST types.  It compiles AST
;;; in the compilation context CONTEXT and returns the first
;;; instruction resulting from the compilation.
(defgeneric compile-ast (ast context))

;;; Instructions inherit policies from the AST that birthed them.
(defmethod compile-ast :around ((ast cleavir-ast:ast) context)
  (let ((cleavir-ir:*policy* (cleavir-ast:policy ast))
        (cleavir-ir:*origin* (cleavir-ast:origin ast))
        (cleavir-ir:*dynamic-environment*
          (dynamic-environment context)))
    (call-next-method)))

;;; This :AROUND method serves as an adapter for the compilation of
;;; ASTs that generate a single value.  If such an AST is compiled in
;;; a unfit context (i.e, a context other than one that has a single
;;; successor and a single required value), this method either creates
;;; a perfect context for compiling that AST together with
;;; instructions for satisfying the unfit context, or it signals an
;;; error if appropriate.
(defmethod compile-ast :around ((ast cleavir-ast:one-value-ast-mixin) context)
  (with-accessors ((results results)
		   (successors successors)
		   (invocation invocation))
      context
    (assert-context ast context nil 1)
    (let ((cleavir-ir:*policy* (cleavir-ast:policy ast))
          (cleavir-ir:*origin* (cleavir-ast:origin ast))
          (cleavir-ir:*dynamic-environment*
            (dynamic-environment context)))
      ;; We have a context with one successor, so RESULTS can be a
      ;; list of any length, or it can be a values location,
      ;; indicating that all results are needed.
      (cond ((typep results 'cleavir-ir:values-location)
             ;; The context is such that all multiple values are
             ;; required.
             (let ((temp (make-temp)))
               (call-next-method
                ast
                (clone-context
                 context
                 :results (list temp)
                 :successors (list (cleavir-ir:make-fixed-to-multiple-instruction
                                    (list temp)
                                    results
                                    (first successors)))))))
            ((null results)
             ;; We don't need the result.  This situation typically
             ;; happens when we compile a form other than the last of
             ;; a PROGN-AST.
             (if (cleavir-ast:side-effect-free-p ast)
                 (progn
                   ;; For now, we do not emit this warning.  It is a bit
                   ;; too annoying because there is some automatically
                   ;; generated code that is getting warned about.
                   ;; (warn "Form compiled in a context requiring no value.")
                   (first successors))
                 ;; We allocate a temporary variable to receive the
                 ;; result, and that variable will not be used.
                 (call-next-method ast
                                   (clone-context context
                                                  :results (list (make-temp))))))
            (t
             ;; We have at least one result.  In case there is more
             ;; than one, we generate a successor where all but the
             ;; first one are filled with NIL.
             (let ((successor (nil-fill (rest results) (first successors))))
               (call-next-method ast
                                 (clone-context context
                                                :results (list (first results))
                                                :successors (list successor)))))))))

;;; If these checks fail, it's an internal bug, since the
;;; :around method should fix the results and successors.
(defmethod compile-ast :before
    ((ast cleavir-ast:one-value-ast-mixin) context)
  (assert-context ast context 1 1))

(defmethod compile-ast :before
    ((ast cleavir-ast:no-value-ast-mixin) context)
  (assert-context ast context 0 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile ASTs that represent Common Lisp operations.

;;; Convenience macro for a common kind of AST/instruction.
;;; It is "functional" in that it evaluates its arguments
;;; left to right like a function, and has a fixed number
;;; of arguments.

(defmacro define-compile-functional-ast (ast instruction (&rest ast-readers))
  (let ((temps (loop for reader in ast-readers collect (gensym "TEMP"))))
    ;; We expand into some nested calls.
    ;; For example say we had two readers, arg1-ast and arg2-ast.
    ;; Then we end up with a compile-ast body of
    #+(or)
    (let ((temp1 (make-temp)) (temp2 (make-temp)))
      (compile-ast
       (arg1-ast ast)
       (clone-context
        context
        :results (list temp1)
        :successors
        (list (compile-ast
               (arg2-ast ast)
               (clone-context
                context
                :results (list temp2)
                :successors
                (list (make-instance 'whatever-instruction
                                     :inputs (list temp1 temp2)
                                     :outputs (results context)
                                     :successors (successors context)))))))))
    (labels ((recur (remaining-readers remaining-temps)
               (if (null remaining-readers)
                   `(make-instance ',instruction
                                   :inputs (list ,@temps)
                                   :outputs (results context)
                                   :successors (successors context))
                   (let ((reader (first remaining-readers))
                         (rest-readers (rest remaining-readers))
                         (temp (first remaining-temps))
                         (rest-temps (rest remaining-temps)))
                     `(compile-ast
                       (,reader ast)
                       (clone-context
                        context
                        :results (list ,temp)
                        :successors (list
                                     ,(recur rest-readers rest-temps))))))))
      `(defmethod compile-ast ((ast ,ast) context)
         (let (,@(loop for temp in temps
                       collect `(,temp (make-temp))))
           ,(recur ast-readers temps))))))

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
                 (clone-context
                  context
                  :results nil
                  :successors (list then-branch else-branch)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a BRANCH-AST.

(defmethod compile-ast ((ast cleavir-ast:branch-ast) context)
  (let ((branches (loop for ast in (cleavir-ast:branch-asts ast)
                        collect (compile-ast ast context)))
        (default (compile-ast (cleavir-ast:default-ast ast) context)))
    (compile-ast (cleavir-ast:test-ast ast)
                 (clone-context
                  context
                  :results nil
                  :successors (append branches (list default))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a PROGN-AST.
;;;
;;; The last sub-ast is compiled in the same context as the progn-ast
;;; itself.  All the others are compiled in a context where no value is
;;; required, and with the code for the following form as a single
;;; successor.
;;;
;;; In the process of converting forms to ASTs, we make sure that
;;; every PROGN-AST has at least one FORM-AST in it.  Otherwise a
;;; different AST is generated instead.

(defun compile-sequence-for-effect (asts context)
  (loop for sub-ast in (reverse asts)
        for successor = (compile-ast sub-ast context)
        do (setf context (clone-context context
                                        :successors (list successor))))
  (first (successors context)))

(defmethod compile-ast ((ast cleavir-ast:progn-ast) context)
  (let ((form-asts (cleavir-ast:form-asts ast)))
    (assert (not (null form-asts)))
    (let* ((next (compile-ast (car (last form-asts)) context))
           (context (clone-context context :results '()
                                           :successors (list next))))
      (compile-sequence-for-effect (butlast form-asts) context))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a BLOCK-AST.
;;;
;;; A BLOCK-AST is compiled by inserting a CATCH-INSTRUCTION,
;;; which has as first successor the compilation of the body
;;; in the same context as the block-ast itself, and as second
;;; successor the successor of the block-ast context.
;;; The context, the continuation, and the destination are stored in
;;; the *BLOCK-INFO* hash table using the block-ast as a key, so that
;;; a RETURN-FROM-AST that refers to this block can be compiled
;;; either in the block's context if it's local, and to unwind
;;; using the correct continuation otherwise.

(defparameter *block-info* nil)

(defun block-info (block-ast)
  (gethash block-ast *block-info*))

(defun (setf block-info) (new-info block-ast)
  (setf (gethash block-ast *block-info*) new-info))

(defmethod compile-ast ((ast cleavir-ast:block-ast) context)
  (with-accessors ((results results)
                   (successors successors)
                   (invocation invocation))
      context
    (let* ((after (first successors))
           ;; The name is gone by now, so unlike TAGBODY
           ;; we can't name the catch output.
           (continuation (cleavir-ir:make-lexical-location
                          '#:block-continuation))
           (dynenv-out (cleavir-ir:make-lexical-location
                        '#:block-dynenv))
           (local-unwind
             (let ((cleavir-ir:*dynamic-environment* dynenv-out))
               (cleavir-ir:make-local-unwind-instruction after)))
           (catch (cleavir-ir:make-catch-instruction
                   continuation
                   dynenv-out
                   (list after)))
           (new-context
             (clone-context context
                            :successors (list local-unwind)
                            :dynamic-environment dynenv-out)))
      (setf (block-info ast) (list context continuation catch))
      ;; Now just hook up the catch to go to the body normally.
      (push (compile-ast (cleavir-ast:body-ast ast) new-context)
            (cleavir-ir:successors catch))
      catch)))

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
;;; If we have a non-local control transfer, we must compile the
;;; FORM-AST with the same results as the ones in the context in which
;;; the BLOCK-AST was compiled, but in the current invocation.  We
;;; then insert an UNWIND-INSTRUCTION that serves as the successor of
;;; the compilation of the FORM-AST.  The destination of that
;;; UNWIND-INSTRUCTION is the successor of the context in which the
;;; BLOCK-AST was compiled.

(defmethod compile-ast ((ast cleavir-ast:return-from-ast) context)
  (let* ((block-ast (cleavir-ast:block-ast ast))
         (block-info (block-info block-ast))
         (block-context (first block-info))
         (continuation (second block-info))
         (destination (third block-info)))
    (with-accessors ((results results)
                     (successors successors)
                     (invocation invocation))
        block-context
      (if (eq (invocation context) invocation)
          ;; simple case: we are returning locally.
          (compile-ast
           (cleavir-ast:form-ast ast)
           (clone-context
            context
            :results results
            :successors (list
                         (cleavir-ir:make-local-unwind-instruction
                          (first successors)))))
          ;; harder case: unwind.
	  (let* ((new-successor (cleavir-ir:make-unwind-instruction
                                 continuation destination 1))
		 (new-context (clone-context
                               context
                               :successors (list new-successor)
                               :results results)))
	    (compile-ast (cleavir-ast:form-ast ast) new-context))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a TAGBODY-AST.
;;;
;;; The TAGBODY-AST is always the first AST of two ASTs in a
;;; PROGN-AST.  The second AST in the PROGN-AST is a CONSTANT-AST
;;; containing NIL.  Therefore, we know that the TAGBODY-AST is always
;;; compiled in a context where no values are required and that has a
;;; single successor.

;;; During AST-to-HIR translation, this variable contains a hash table
;;; that maps a TAG-AST to information about the tag.  The information
;;; is a cons.  The car is the NOP instruction that will be the target
;;; of any GO instruction to that tag.  The second element is the
;;; INVOCATION of the compilation context, which is used to determine
;;; whether the GO to this tag is local or non-local.
(defparameter *go-info* nil)

(defun go-info (tag-ast)
  (gethash tag-ast *go-info*))

(defun (setf go-info) (new-info tag-ast)
  (setf (gethash tag-ast *go-info*) new-info))

;;; What we end up with is 1 CATCH instruction, which has one successor for each
;;; tag plus one, which is followed by the non-tag items compiled in order like
;;; a PROGN, but in a special context that includes info about the tags.
;;; The CATCH instruction abnormal successors are NOPs that have the appropriate
;;; items as successors.
;;; To accomplish this, we make the main NOP, then loop over the body twice.
(defmethod compile-ast ((ast cleavir-ast:tagbody-ast) context)
  (with-accessors ((results results)
                   (successors successors)
                   (invocation invocation))
      context
    (let* ((continuation (cleavir-ir:make-lexical-location
                          '#:tagbody-continuation))
           (dynenv-out (cleavir-ir:make-lexical-location
                        '#:tagbody-dynenv))
           (catch (cleavir-ir:make-catch-instruction
                   continuation dynenv-out nil))
           (catch-successors nil)
           (cleavir-ir:*dynamic-environment* dynenv-out))
      ;; In the first loop, we make a NOP for each tag, which will be the
      ;; destination for any GO or UNWIND to that tag. It's put in the go-info
      ;; with the invocation, and also put as one of the catch's successors.
      (loop with index = 0
            for item-ast in (cleavir-ast:item-asts ast)
            when (typep item-ast 'cleavir-ast:tag-ast)
              do (let ((nop (cleavir-ir:make-nop-instruction nil)))
                   (push nop catch-successors)
                   (incf index)
                   (setf (go-info item-ast) (list invocation continuation nop catch index))))
      ;; Now we actually compile the items, in reverse order (like PROGN).
      (loop with next = (let ((cleavir-ir:*dynamic-environment* dynenv-out))
                          (cleavir-ir:make-local-unwind-instruction
                           (first successors)))
            for item-ast in (reverse (cleavir-ast:item-asts ast))
            ;; if an item is a tag, we set the catch's NOP to succeed
            ;; to the right place (the current NEXT).
            ;; We also include the NOP in the normal sequence (i.e. make it NEXT).
            ;; This isn't strictly necessary, but if we don't it makes a pointless
            ;; basic block.
            if (typep item-ast 'cleavir-ast:tag-ast)
              do (let ((nop (third (go-info item-ast))))
                   (setf (cleavir-ir:successors nop) (list next)
                         next nop))
            ;; if it's not a tag, we compile it, expecting no values.
            else do (setf next
                          (compile-ast item-ast
                                       (clone-context
                                        context
                                        :results '()
                                        :successors (list next)
                                        :dynamic-environment dynenv-out)))
            ;; lastly we hook up the main CATCH to go to the item code the first
            ;; time through. (As the first successor.)
            finally (setf (cleavir-ir:successors catch) (list next)))
      (setf (rest (cleavir-ir:successors catch)) (nreverse catch-successors))
      catch)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a GO-AST.
;;;
;;; We obtain the GO-INFO that was stored when the TAG-AST of this
;;; GO-AST was compiled.  This info is a cons. The CAR is the CATCH
;;; instruction for the tag. The CDR is the INVOCATION of the tagbody.
;;;
;;; The INVOCATION of the parameter CONTEXT is compared to the
;;; tagbody invocation. If they are the same, we have a local transfer
;;; of control, so we just return the NOP instruction that the CATCH
;;; has as its abnormal successor. If they are not the same, we generate
;;; an UNWIND-INSTRUCTION with the CATCH as its destination and using its
;;; continuation.

(defmethod compile-ast ((ast cleavir-ast:go-ast) context)
  (let* ((info (go-info (cleavir-ast:tag-ast ast)))
         (invocation (first info)) (continuation (second info))
         (nop (third info))
         (destination (fourth info)) (index (fifth info)))
    (if (eq invocation (invocation context))
        (cleavir-ir:make-local-unwind-instruction nop)
	(cleavir-ir:make-unwind-instruction
         continuation destination index))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a CALL-AST.

(defmethod compile-ast ((ast cleavir-ast:call-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (assert-context ast context nil 1)
    (let* ((all-args (cons (cleavir-ast:callee-ast ast)
                           (cleavir-ast:argument-asts ast)))
           ;; Could do more sophisticated analysis for whether to
           ;; mark an instruction as inlinable.
           (inline (cleavir-ast:inline-declaration ast))
	   (temps (make-temps all-args))
           ;; In case they diverge at some point.
           (inputs temps))
      (compile-arguments
       all-args
       temps
       (if (typep results 'cleavir-ir:values-location)
	   (make-instance 'cleavir-ir:funcall-instruction
	     :inputs inputs
	     :outputs (list results)
	     :successors successors
             :inline inline)
	   (let* ((values-temp (make-instance 'cleavir-ir:values-location)))
	     (make-instance 'cleavir-ir:funcall-instruction
	       :inputs inputs
	       :outputs (list values-temp)
	       :successors
	       (list (cleavir-ir:make-multiple-to-fixed-instruction
		      values-temp results (first successors)))
               :inline inline)))
       context))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FUNCTION-AST.
;;;
;;; The FUNCTION-AST represents a closure, so we compile it by
;;; compiling its LAMBDA-LIST and BODY-AST into some code, represented
;;; by the first instruction in the body.  We then generate an
;;; ENCLOSE-INSTRUCTION that takes this code as input.
;;;
;;; With inlining, it is possible for the same FUNCTION-AST to be
;;; encountered more than once. This represents a separate enclosure
;;; of the same function. We maintain a table of encountered FUNCTION-ASTs
;;; to avoid recompiling any.
;;; (If we don't, it causes numerous inconsistencies. Most especially,
;;;  LEXICAL-ASTs will compile to the same datum, resulting in data with
;;;  no one owner.)

(defun translate-lambda-list (lambda-list)
  (loop for item in lambda-list
	collect (cond ((member item lambda-list-keywords)
		       item)
		      ((consp item)
		       (if (= (length item) 3)
			   (list (first item)
				 (find-or-create-location (second item))
				 (find-or-create-location (third item)))
			   (list (find-or-create-location (first item))
				 (find-or-create-location (second item)))))
		      (t
		       (find-or-create-location item)))))

(defun translate-bound-declarations (bound-declarations)
  (loop for (lexical-ast . decls) in bound-declarations
        collect (cons (find-or-create-location lexical-ast) decls)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function COMPILE-FUNCTION
;;;
;;; Returns a new enter instruction and so on for a
;;; FUNCTION-AST. Users that subclass FUNCTION-AST
;;; can define methods on it.

(defgeneric compile-function (ast))

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
(defmethod compile-function ((ast cleavir-ast:function-ast))
  (let* ((ll (translate-lambda-list (cleavir-ast:lambda-list ast)))
         (bd (translate-bound-declarations
              (cleavir-ast:bound-declarations ast)))
         (dynenv (cleavir-ir:make-lexical-location
                  '#:function-dynenv))
         (cleavir-ir:*dynamic-environment* dynenv)
         ;; Note the ENTER gets its own output as its dynamic environment.
         (enter (cleavir-ir:make-enter-instruction
                 ll dynenv
                 :name (cleavir-ast:name ast)
                 :docstring (cleavir-ast:docstring ast)
                 :original-lambda-list (cleavir-ast:original-lambda-list ast)
                 :bound-declarations bd
                 :origin (cleavir-ast:origin ast)))
         (values (cleavir-ir:make-values-location))
         (return (cleavir-ir:make-return-instruction (list values)))
         (body-context (context values (list return) enter dynenv))
         (body (compile-ast (cleavir-ast:body-ast ast) body-context)))
    (reinitialize-instance enter :successors (list body))
    enter))

(defvar *function-info*)

(defmethod compile-ast ((ast cleavir-ast:function-ast) context)
  ;; As per above comment concerning inlining, we memoize here.
  (let ((enter (or (gethash ast *function-info*)
                   (setf (gethash ast *function-info*)
                         (compile-function ast)))))
    (cleavir-ir:make-enclose-instruction
     (first (results context))
     (first (successors context))
     enter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a SETQ-AST.

(defmethod compile-ast ((ast cleavir-ast:setq-ast) context)
  (let ((location (find-or-create-location (cleavir-ast:lhs-ast ast))))
    (compile-ast
     (cleavir-ast:value-ast ast)
     (clone-context context :results (list location)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a MULTIPLE-VALUE-PROG1-AST.

(defmethod compile-ast ((ast cleavir-ast:multiple-value-prog1-ast) context)
  (compile-ast
   (cleavir-ast:first-form-ast ast)
   (clone-context
    context
    :successors
    (list
     (if (typep (results context) 'cleavir-ir:values-location)
         (let* ((dynenv (cleavir-ir:make-lexical-location
                         '#:saved-values-dynenv))
                (body-context
                  (clone-context
                   context
                   :results '()
                   :dynamic-environment dynenv
                   :successors
                   (list (let ((cleavir-ir:*dynamic-environment* dynenv))
                           (cleavir-ir:make-local-unwind-instruction
                            (first (successors context))))))))
          (make-instance 'cleavir-ir:save-values-instruction
            :outputs (list dynenv)
            :successors
            (list
             (compile-sequence-for-effect
              (cleavir-ast:form-asts ast)
              body-context))))
         (compile-sequence-for-effect
          (cleavir-ast:form-asts ast)
          (clone-context context :results '())))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a MULTIPLE-VALUE-EXTRACT-AST.

(defmethod compile-ast ((ast cleavir-ast:multiple-value-extract-ast) context)
  (let ((results (results context))
        (locations (mapcar #'find-or-create-location
                           (cleavir-ast:lhs-asts ast))))
    (cond
      ((typep results 'cleavir-ir:values-location)
       (let* ((dynenv (cleavir-ir:make-lexical-location
                       '#:saved-values-dynenv))
              (body-context
                (clone-context
                 context
                 :results '()
                 :dynamic-environment dynenv
                 :successors
                 (list (let ((cleavir-ir:*dynamic-environment* dynenv))
                         (cleavir-ir:make-local-unwind-instruction
                          (first (successors context)))))))
              (succ
                (make-instance 'cleavir-ir:save-values-instruction
                  :outputs (list dynenv)
                  :successors
                  (list
                   (make-instance 'cleavir-ir:multiple-to-fixed-instruction
                     :inputs (list results)
                     :outputs locations
                     :dynamic-environment dynenv
                     :successors
                     (list
                      (compile-sequence-for-effect (cleavir-ast:form-asts ast)
                                                   body-context)))))))
         (compile-ast
          (cleavir-ast:first-form-ast ast)
          (clone-context
           context
           :successors (list succ)))))
      (t
       (when (>= (length locations) (length results))
         (rotatef locations results))
       (let* ((successor
                (compile-sequence-for-effect (cleavir-ast:form-asts ast)
                                             (clone-context context
                                                            :results '())))
              (succ (loop with succ = successor
                          for location in locations
                          for result in results
                          do (setf succ
                                   (cleavir-ir:make-assignment-instruction
                                    result location succ))
                          finally (return succ))))
         (compile-ast
          (cleavir-ast:first-form-ast ast)
          (clone-context
           context
           :results results
           :successors (list succ))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a MULTIPLE-VALUE-SETQ-AST.

(defmethod compile-ast ((ast cleavir-ast:multiple-value-setq-ast) context)
  (assert-context ast context 0 1)
  (let ((locations (mapcar #'find-or-create-location
                           (cleavir-ast:lhs-asts ast))))
    (compile-ast
     (cleavir-ast:form-ast ast)
     (clone-context context :results locations))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a THE-AST.

(defun make-type-check (type-specifier var successor)
  (cleavir-ir:make-the-instruction var successor type-specifier))

(defmethod compile-ast ((ast cleavir-ast:the-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (assert-context ast context nil 1)
    (let ((form-ast (cleavir-ast:form-ast ast))
	  (required (cleavir-ast:required-types ast))
	  (optional (cleavir-ast:optional-types ast))
	  (rest (cleavir-ast:rest-type ast))
	  (successor (first successors)))
      (cond
	((typep results 'cleavir-ir:values-location)
	 (compile-ast form-ast
                      (clone-context
                       context
                       :successors (list
                                    (cleavir-ir:make-the-values-instruction
                                     results successor
                                     required optional rest)))))
	(t ; lexical locations
	 (loop for lex in results
	       do (setf successor
			(cleavir-ir:make-the-instruction
			 lex successor (cond
					 (required (pop required))
					 (optional (pop optional))
					 (t rest)))))
	 (compile-ast form-ast
                      (clone-context context :successors (list successor))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a DYNAMIC-ALLOCATION-AST.

(defmethod compile-ast ((ast cleavir-ast:dynamic-allocation-ast)
                        context)
  (with-accessors ((results results)
                   (successors successors)
                   (invocation invocation))
      context
    (assert-context ast context nil 1)
    ;; It's a ONE-VALUE-AST-MIXIN, so RESULTS is one lexical loc.
    (compile-ast
     (cleavir-ast:form-ast ast)
     (clone-context
      context
      :successors (list
                   (cleavir-ir:make-dynamic-allocation-instruction
                    (first results) (first successors)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile an UNREACHABLE-AST.

(defmethod compile-ast ((ast cleavir-ast:unreachable-ast) context)
  ;; Code like (foo (unreachable)) is possible. In this case
  ;; the context will be expecting a value. But we don't have
  ;; anything to assign. So the location will be uninitialized.
  ;; To avoid any possibility of an instruction using an
  ;; uninitialized location it's better to cut that instruction off
  ;; entirely, i.e. make it not a successor.
  (cleavir-ir:make-unreachable-instruction))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a SYMBOL-VALUE-AST.

(define-compile-functional-ast
    cleavir-ast:symbol-value-ast cleavir-ir:symbol-value-instruction
  (cleavir-ast:symbol-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a CONSTANT-SYMBOL-VALUE-AST.

(defmethod compile-ast ((ast cleavir-ast:constant-symbol-value-ast) context)
  (cleavir-ir:make-constant-symbol-value-instruction
   (cleavir-ast:name ast)
   (first (results context))
   (first (successors context))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a SET-SYMBOL-VALUE-AST.

(define-compile-functional-ast
    cleavir-ast:set-symbol-value-ast cleavir-ir:set-symbol-value-instruction
  (cleavir-ast:symbol-ast cleavir-ast:value-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a SET-CONSTANT-SYMBOL-VALUE-AST.

(defmethod compile-ast ((ast cleavir-ast:set-constant-symbol-value-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (let ((temp (make-temp)))
      (compile-ast
       (cleavir-ast:value-ast ast)
       (clone-context
        context
        :results (list temp)
        :successors (list (cleavir-ir:make-set-constant-symbol-value-instruction
                           (cleavir-ast:name ast)
                           temp
                           (first successors))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FDEFINITION-AST.

(define-compile-functional-ast
    cleavir-ast:fdefinition-ast cleavir-ir:fdefinition-instruction
  (cleavir-ast:name-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a CONSTANT-FDEFINITION-AST.

(defmethod compile-ast ((ast cleavir-ast:constant-fdefinition-ast) context)
  (cleavir-ir:make-constant-fdefinition-instruction
   (cleavir-ast:name ast)
   (first (results context))
   (first (successors context))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a TYPEQ-AST.

(defmethod compile-ast ((ast cleavir-ast:typeq-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (assert-context ast context 0 2)
    (let ((temp (make-temp)))
      (compile-ast
       (cleavir-ast:form-ast ast)
       (clone-context
        context
        :results (list temp)
	:successors (list (cleavir-ir:make-typeq-instruction
                           temp
                           successors
                           (cleavir-ast:type-specifier ast))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a TYPEW-AST.
;;;
;;; TYPEW instructions are unusual in having three successors
;;; despite appearing in a normal boolean position. This is
;;; because their test-ast is used as another boolean test with
;;; the same destinations (plus the choke).

(defmethod compile-ast ((ast cleavir-ast:typew-ast) context)
  (assert-context ast context 0 2)
  (let* ((successors (successors context))
         (real-test-context
           (clone-context
            context
            :successors (list (cleavir-ir:make-choke-instruction
                               (first successors))
                              (cleavir-ir:make-choke-instruction
                               (second successors)))))
         (real-test
           (compile-ast (cleavir-ast:test-ast ast)
                        real-test-context)))
    (cleavir-ir:make-typew-instruction
     (find-or-create-location (cleavir-ast:variable-ast ast))
     (list (first successors) (second successors) real-test)
     (cleavir-ast:ctype ast))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a LEXICAL-AST.
;;;
;;; This AST has ONE-VALUE-AST-MIXIN as a superclass.

(defmethod compile-ast ((ast cleavir-ast:lexical-ast) context)
  (cleavir-ir:make-assignment-instruction
   (find-or-create-location ast)
   (first (results context))
   (first (successors context))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COMPILE-TOPLEVEL
;;;
;;; This is the main entry point.

(defun compile-toplevel (ast)
  (let ((*block-info* (make-hash-table :test #'eq))
	(*go-info* (make-hash-table :test #'eq))
	(*location-info* (make-hash-table :test #'eq))
        (*function-info* (make-hash-table :test #'eq))
        (cleavir-ir:*origin* (cleavir-ast:origin ast))
	(cleavir-ir:*policy* (cleavir-ast:policy ast)))
    (check-type ast cleavir-ast:top-level-function-ast)
    (let* ((ll (translate-lambda-list (cleavir-ast:lambda-list ast)))
           (dynenv (cleavir-ir:make-lexical-location
                    '#:toplevel-function-dynenv))
	   (forms (cleavir-ast:forms ast))
           (cleavir-ir:*dynamic-environment* dynenv)
	   (enter (cleavir-ir:make-top-level-enter-instruction ll forms dynenv
                                                               :origin (cleavir-ast:origin ast)))
	   (values (cleavir-ir:make-values-location))
	   (return (cleavir-ir:make-return-instruction (list values)))
	   (body-context (context values (list return) enter dynenv))
	   (body (compile-ast (cleavir-ast:body-ast ast) body-context)))
      ;; Now we must set the successors of the ENTER-INSTRUCTION to a
      ;; list of the result of compiling the AST.
      (reinitialize-instance enter :successors (list body))
      ;; Make sure the list of predecessors of each instruction is
      ;; initialized correctly.
      (cleavir-ir:set-predecessors enter)
      enter)))

(defun compile-toplevel-unhoisted (ast)
  (let ((*block-info* (make-hash-table :test #'eq))
	(*go-info* (make-hash-table :test #'eq))
	(*location-info* (make-hash-table :test #'eq))
        (*function-info* (make-hash-table :test #'eq))
        (cleavir-ir:*origin* (cleavir-ast:origin ast))
	(cleavir-ir:*policy* (cleavir-ast:policy ast)))
    (let* ((ll (translate-lambda-list (cleavir-ast:lambda-list ast)))
           (bd (translate-bound-declarations
                (cleavir-ast:bound-declarations ast)))
           (dynenv (cleavir-ir:make-lexical-location
                    '#:toplevel-unhoisted-function-dynenv))
           (cleavir-ir:*dynamic-environment* dynenv)
	   (enter (cleavir-ir:make-enter-instruction
                   ll dynenv
                   :bound-declarations bd
                   :name (cleavir-ast:name ast)
                   :docstring (cleavir-ast:docstring ast)
                   :original-lambda-list (cleavir-ast:original-lambda-list ast)
                   :origin (cleavir-ast:origin ast)))
	   (values (cleavir-ir:make-values-location))
	   (return (cleavir-ir:make-return-instruction (list values)))
	   (body-context (context values (list return) enter dynenv))
	   (body (compile-ast (cleavir-ast:body-ast ast) body-context)))
      ;; Now we must set the successors of the ENTER-INSTRUCTION to a
      ;; list of the result of compiling the AST.
      (reinitialize-instance enter :successors (list body))
      ;; Make sure the list of predecessors of each instruction is
      ;; initialized correctly.
      (cleavir-ir:set-predecessors enter)
      enter)))

;;; When the FUNCTION-AST is in fact a TOP-LEVEL-FUNCTION-AST it also
;;; contains a list of LOAD-TIME-VALUE forms to be evaluated and then
;;; supplied as arguments to the function.  We need to preserve that
;;; list of forms which we can do by producing an instance of
;;; TOP-LEVEL-ENTER-INSTRUCTION rather than of a plain
;;; ENTER-INSTRUCTION.  For that reason, we define an :AROUND method
;;; on that type of AST that turns the ENTER-INSTRUCTION into a
;;; TOP-LEVEL-ENTER-INSTRUCTION.

(defmethod compile-ast :around ((ast cleavir-ast:top-level-function-ast) context)
  (declare (ignore context))
  (let* ((enclose (call-next-method))
	 (enter (cleavir-ir:code enclose)))
    (change-class enter 'cleavir-ir:top-level-enter-instruction
		  :forms (cleavir-ast:forms ast))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile ASTs that represent low-level operations.

(defun make-temps (arguments)
  (loop for argument in arguments
	collect (make-temp)))

(defun compile-arguments (arguments temps successor context)
  (loop with succ = successor
	for arg in (reverse arguments)
	for temp in (reverse temps)
	do (setf succ (compile-ast arg
                                   (clone-context
                                    context
                                    :results `(,temp)
                                    :successors `(,succ))))
	finally (return succ)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a IMMEDIATE-AST.
;;;
;;; The IMMEDIATE-AST is a subclass of ONE-VALUE-AST-MIXIN, so the
;;; :AROUND method on COMPILE-AST has adapted the context so that it
;;; has a single result.

(defmethod compile-ast ((ast cleavir-ast:immediate-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (cleavir-ir:make-assignment-instruction
     (cleavir-ir:make-immediate-input (cleavir-ast:value ast))
     (first results)
     (first successors))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a MULTIPLE-VALUE-CALL-AST.

(defmethod compile-ast ((ast cleavir-ast:multiple-value-call-ast) context)
  (with-accessors ((results results)
		   (successors successors)
		   (invocation invocation))
      context
    (assert-context ast context nil 1)
    (let* ((function-temp (cleavir-ir:new-temporary))
           (form-temps (loop repeat (length (cleavir-ast:form-asts ast))
                             collect (cleavir-ir:make-values-location)))
           (inputs (list* function-temp form-temps))
           (successor
             (if (typep results 'cleavir-ir:values-location)
                 (make-instance 'cleavir-ir:multiple-value-call-instruction
                   :inputs inputs
                   :outputs (list results)
                   :successors successors)
                 (let* ((values-temp (make-instance 'cleavir-ir:values-location)))
                   (make-instance 'cleavir-ir:multiple-value-call-instruction
                     :inputs inputs
                     :outputs (list values-temp)
                     :successors
                     (list (cleavir-ir:make-multiple-to-fixed-instruction
                            values-temp results (first successors))))))))
	(loop for form-ast in (reverse (cleavir-ast:form-asts ast))
	      for form-temp in (reverse form-temps)
	      do (setf successor
		       (compile-ast
			form-ast
                        (clone-context context
                                       :results form-temp
                                       :successors (list successor)))))
	(compile-ast
	 (cleavir-ast:function-form-ast ast)
         (clone-context context
                        :results (list function-temp)
                        :successors (list successor))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a VALUES-AST.

(defun split-lists (list1 list2)
  ;; Returns maximal prefixes of the same length,
  ;; and then suffixes. E.g.:
  ;; (s-l '(1 2) '(3 4 5)) => (1 2), (3 4), nil, (5)
  ;; (s-l '(1 2 3) '(4 5)) => (1 2), (4 5), (3), nil
  ;; (s-l '(1 2) '(3 4)) => (1 2), (3 4), nil, nil
  (let ((min (min (length list1) (length list2))))
    (values (subseq list1 0 min)
	    (subseq list2 0 min)
	    (nthcdr min list1)
	    (nthcdr min list2))))

(defmethod compile-ast ((ast cleavir-ast:values-ast) context)
  (with-accessors ((results results)
		   (successors successors)
		   (invocation invocation))
      context
    (assert-context ast context nil 1)
    (let ((arguments (cleavir-ast:argument-asts ast)))
      (cond ((typep results 'cleavir-ir:values-location)
	     (let ((temps (make-temps arguments)))
	       (compile-arguments
		arguments temps
		(cleavir-ir:make-fixed-to-multiple-instruction
		 temps results (first successors))
                context)))
	    (t ;lexical locations
	     ;; this is a bit tricky because there may be more or less
	     ;; arguments than results, in which case we must compile
	     ;; for effect or nil-fill.
	     ;; First we collect those that match.
	     (multiple-value-bind (args results valueless nils)
		 (split-lists arguments results)
	       ;; Now we know which match, so compile those.
	       ;; Note also we have to preserve left-to-right evaluation order.
	       (compile-arguments
		args
		results
		;; Obviously at most one of valueless and nils can be non-null.
		(cond (valueless
		       (loop with succ = (first successors)
			     for arg in (reverse valueless)
			     do (setf succ (compile-ast arg
                                                        (clone-context
                                                         context
                                                         :results '()
                                                         :successors (list succ))))
			     finally (return succ)))
		      (nils (nil-fill nils (first successors)))
		      (t (first successors)))
                context)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile an EQ-AST.

(defmethod compile-ast ((ast cleavir-ast:eq-ast) context)
  (with-accessors ((successors successors)
		   (invocation invocation))
      context
    (ecase (length successors)
      (1
       (let ((cleavir-ast:*policy* (cleavir-ast:policy ast)))
         (compile-ast (cleavir-ast:make-if-ast
                       ast
                       (cleavir-ast:make-load-time-value-ast (list 'quote t))
                       (cleavir-ast:make-load-time-value-ast (list 'quote nil)))
                      context)))
      (2
       (assert-context ast context 0 2)
       (let ((arg1-ast (cleavir-ast:arg1-ast ast))
	     (arg2-ast (cleavir-ast:arg2-ast ast))
	     (temp1 (cleavir-ir:new-temporary))
	     (temp2 (cleavir-ir:new-temporary)))
         (compile-ast
          arg1-ast
          (clone-context
           context
           :results (list temp1)
           :successors (list (compile-ast
                              arg2-ast
                              (clone-context
                               context
                               :results (list temp2)
                               :successors (list
                                            (cleavir-ir:make-eq-instruction
                                             (list temp1 temp2)
                                             successors))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a NEQ-AST.

(defmethod compile-ast ((ast cleavir-ast:neq-ast) context)
  (assert-context ast context 0 2)
  (with-accessors ((successors successors)
		   (invocation invocation))
      context
    (let ((arg1-ast (cleavir-ast:arg1-ast ast))
          (arg2-ast (cleavir-ast:arg2-ast ast))
          (temp1 (cleavir-ir:new-temporary))
          (temp2 (cleavir-ir:new-temporary)))
      (compile-ast
       arg1-ast
       (clone-context
        context
        :results (list temp1)
        :successors (list
                     (compile-ast
                      arg2-ast
                      (clone-context
                       context
                       :results (list temp2)
                       :successors (list
                                    (cleavir-ir:make-eq-instruction
                                     (list temp1 temp2)
                                     ;; Reverse successors.
                                     (list (second successors)
                                           (first successors))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a CASE-AST.

(defmethod compile-ast ((ast cleavir-ast:case-ast) context)
  (let ((arg-ast (cleavir-ast:arg-ast ast))
        (comparees (cleavir-ast:comparees ast))
        (temp (cleavir-ir:new-temporary)))
    (assert-context ast context 0 (1+ (length comparees)))
    (compile-ast
     arg-ast
     (clone-context
      context
      :results (list temp)
      :successors (list (cleavir-ir:make-case-instruction
                         temp
                         comparees
                         (successors context)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a LOAD-TIME-VALUE-AST.
;;;
;;; The LOAD-TIME-VALUE-AST is a subclass of ONE-VALUE-AST-MIXIN, so
;;; the :AROUND method on COMPILE-AST has adapted the context so that
;;; it has a single result.

(defmethod compile-ast ((ast cleavir-ast:load-time-value-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (cleavir-ir:make-assignment-instruction
     (cleavir-ir:make-load-time-value-input
      (cleavir-ast:form ast) (cleavir-ast:read-only-p ast))
     (first results)
     (first successors))))
