(cl:in-package #:cleavir-ast-to-hir)

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
                       client
                       (,reader ast)
                       (clone-context
                        context
                        :result ,temp
                        :successor ,(recur rest-readers rest-temps)))))))
      `(defmethod compile-ast (client (ast ,ast) context)
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

(defmethod compile-ast (client (ast cleavir-ast:if-ast) context)
  (let ((then-branch (compile-ast client (cleavir-ast:then-ast ast) context))
        (else-branch (compile-ast client (cleavir-ast:else-ast ast) context)))
    (compile-ast client
                 (cleavir-ast:test-ast ast)
                 (clone-context
                  context
                  :results '()
                  :successors (list then-branch else-branch)))))

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

(defmethod compile-ast (client (ast cleavir-ast:progn-ast) context)
  (let ((form-asts (cleavir-ast:form-asts ast)))
    (assert (not (null form-asts)))
    (let ((next (compile-ast client (car (last form-asts)) context)))
      (loop for sub-ast in (cdr (reverse (cleavir-ast:form-asts ast)))
            do (setf next
                     (compile-ast client
                                  sub-ast
                                  (clone-context
                                   context
                                   :results '()
                                   :successor next))))
      next)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a BLOCK-AST.
;;;
;;; The code generated from the BLOCK-AST consists of a
;;; CATCH-INSTRUCTION with two successors.  The first successor is the
;;; code resulting from the compilation of the BODY-AST of the
;;; BLOCK-AST.  The second successor is the successor of the context
;;; in which the BLOCK-AST is compiled, i.e. the code to be executed
;;; after the block.  The BODY-AST is compiled in a context where the
;;; dynamic environment is augmented with respect to the dynamic
;;; environment in which the BLOCK-AST is compiled.  The lexical
;;; location that holds this augmented dynamic environment becomes the
;;; second output of the CATCH-INSTRUCTION.
;;;
;;; A hash table in *BLOCK-INFO* is used to store information about
;;; the BLOCK-AST being compiled.  The information consists of a list
;;; of three elements.  The first element is the context in which the
;;; BLOCK-AST is compiled.  This information is used when a
;;; RETURN-FROM-AST for this BLOCK-AST is compiled.  Specifically, the
;;; RESULTS information of that context is used to compile the
;;; FORM-AST of the RETURN-FROM-AST so as to get the values of the
;;; RETURN-FROM-AST in the right places.  The second element of the
;;; list is called the CONTINUATION, and it is a lexical location that
;;; becomes the first output of the CATCH-INSTRUCTION.  At run-time,
;;; the CONTINUATION indicates dynamic information such as register
;;; contents that is required to restore the execution state to what
;;; it needs to be in order to execute the code following the block.
;;; The UNWIND-INSTRUCTION has this CONTINUATION location as its
;;; input.  Finally, the third element of the list is the
;;; CATCH-INSTRUCTION itself, called the DESTINATION.  It is stored in
;;; a slot of the UNWIND-INSTRUCTION.

(defvar *block-info*)

(defun block-info (block-ast)
  (gethash block-ast *block-info*))

(defun (setf block-info) (new-info block-ast)
  (setf (gethash block-ast *block-info*) new-info))

(defmethod compile-ast (client (ast cleavir-ast:block-ast) context)
  (with-accessors ((results results)
                   (successors successors))
      context
    (let* ((after (first successors))
           ;; The name is gone by now, so unlike TAGBODY
           ;; we can't name the catch output.
           (continuation (cleavir-ir:make-lexical-location
                          '#:block-continuation))
           (dynenv-out (cleavir-ir:make-lexical-location (gensym "block")))
           (catch (make-instance 'cleavir-ir:catch-instruction
                   :outputs (list continuation dynenv-out)
                   :successors (list after)))
           (new-context (clone-context
                         context
                         :dynamic-environment-location dynenv-out)))
      (setf (block-info ast) (list context continuation catch))
      ;; Now just hook up the catch to go to the body normally.
      (push (compile-ast client (cleavir-ast:body-ast ast) new-context)
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
;;; the compilation of the FORM-AST.  The DESTINATION of that
;;; UNWIND-INSTRUCTION is the CATCH-INSTRUCTION that resulted from the
;;; compilation of the corresponding BLOCK-AST.

(defmethod compile-ast (client (ast cleavir-ast:return-from-ast) context)
  (let* ((block-info (block-info (cleavir-ast:block-ast ast)))
         (block-context (first block-info))
         (continuation (second block-info))
         (destination (third block-info)))
    (with-accessors ((successors successors)
                     (invocation invocation))
        block-context
      (if (eq (invocation context) invocation)
          ;; simple case: we are returning locally.
          (compile-ast client
                       (cleavir-ast:form-ast ast)
                       (clone-context
                        context
                        :results (results block-context)
                        :successors successors))
          ;; harder case: unwind.
          (let* ((new-successor (make-instance 'cleavir-ir:unwind-instruction
                                  :input continuation
                                  :destination destination
                                  :index 1))
                 (new-context (clone-context
                               context
                               :results (results block-context)
                               :successor new-successor)))
            (compile-ast client (cleavir-ast:form-ast ast) new-context))))))

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
;;; about the tag is a list of five elements: INVICATION,
;;; CONTINUATION, NOP, CATCH, and INDEX.  The INVOCATION of the
;;; compilation context is used to determine whether the GO to this
;;; tag is local or non-local.
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
(defmethod compile-ast (client (ast cleavir-ast:tagbody-ast) context)
  (with-accessors ((results results)
                   (successors successors)
                   (invocation invocation))
      context
    (let* ((continuation (cleavir-ir:make-lexical-location
                          '#:tagbody-continuation))
           (dynenv-out (cleavir-ir:make-lexical-location (gensym "tagbody")))
           (catch (make-instance 'cleavir-ir:catch-instruction
                    :outputs (list continuation dynenv-out)
                    :successors '()))
           (catch-successors nil))
      ;; In the first loop, we make a NOP for each tag, which will be
      ;; the destination for any GO or UNWIND to that tag.  The NOP is
      ;; put in the go-info with the invocation, and also put as one
      ;; of the catch's successors.
      (loop with index = 0
            for item-ast in (cleavir-ast:item-asts ast)
            when (typep item-ast 'cleavir-ast:tag-ast)
              do (let ((nop (make-instance 'cleavir-ir:nop-instruction
                              :successors '()
                              :dynamic-environment-location dynenv-out)))
                   (push nop catch-successors)
                   (incf index)
                   (setf (go-info item-ast)
                         (list invocation continuation nop catch index))))
      ;; Now we actually compile the items, in reverse order (like PROGN).
      (loop with next = (first successors)
            for item-ast in (reverse (cleavir-ast:item-asts ast))
            ;; If an item is a tag, we set the successor of the
            ;; correspondign NOP of the CATCH to the right place (the
            ;; current NEXT).  We also include the NOP in the normal
            ;; sequence (i.e. make it NEXT).  This isn't strictly
            ;; necessary, but if we don't it makes a pointless basic
            ;; block.
            if (typep item-ast 'cleavir-ast:tag-ast)
              do (let ((nop (third (go-info item-ast))))
                   (setf (cleavir-ir:successors nop) (list next)
                         next nop))
            ;; If it's not a tag, we compile it, expecting no values.
            else do (setf next
                          (compile-ast client
                                       item-ast
                                       (clone-context
                                        context
                                        :results '()
                                        :successor next
                                        :dynamic-environment-location dynenv-out)))
            ;; Lastly we hook up the main CATCH to go to the item code
            ;; the first time through. (As the first successor.)
            finally (setf (cleavir-ir:successors catch) (list next)))
      (setf (rest (cleavir-ir:successors catch)) (nreverse catch-successors))
      catch)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a GO-AST.
;;;
;;; The INVOCATION of the parameter CONTEXT is compared to the
;;; tagbody invocation. If they are the same, we have a local transfer
;;; of control, so we just return the NOP instruction that the CATCH
;;; has as its abnormal successor. If they are not the same, we generate
;;; an UNWIND-INSTRUCTION with the CATCH as its destination and using its
;;; continuation.

(defmethod compile-ast (client (ast cleavir-ast:go-ast) context)
  (destructuring-bind (invocation continuation nop destination index)
      (go-info (cleavir-ast:tag-ast ast))
    (if (eq invocation (invocation context))
        nop
        (make-instance 'cleavir-ir:unwind-instruction
          :input continuation
          :destination destination
          :index index))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a CATCH-AST.

(defmethod compile-ast (client (ast cleavir-ast:catch-ast) context)
  (with-accessors ((results results)
                   (successors successors))
      context
    (let ((tag-temp (make-temp))
          (throw-function-temp (make-temp)))
      (compile-ast
       client
       (cleavir-ast:tag-ast ast)
       (clone-context
        context
        :result tag-temp
        :successor
        (compile-ast
         client
         (cleavir-ast:throw-function-ast ast)
         (clone-context
          context
          :result throw-function-temp
          :successor
          (let* ((dynenv-out (cleavir-ir:make-lexical-location (gensym "catch")))
                 (new-context (clone-context
                               context
                               :successor (first successors)
                               :dynamic-environment-location dynenv-out))
                 (body-successor
                   (compile-ast client (cleavir-ast:body-ast ast) new-context)))
            (make-instance 'cleavir-ir:dynamic-catch-instruction
              :inputs (list tag-temp throw-function-temp)
              :outputs (list dynenv-out)
              :successors (list body-successor))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a BIND-AST.

(defmethod compile-ast (client (ast cleavir-ast:bind-ast) context)
  (let* ((name-temp (make-temp))
         (value-temp (make-temp))
         (dynenv (cleavir-ir:make-lexical-location (gensym "bind")))
         (new-context (clone-context
                       context
                       :dynamic-environment-location dynenv))
         (body (compile-ast client (cleavir-ast:body-ast ast) new-context))
         (wrapped-body (make-instance 'cleavir-ir:bind-instruction
                         :inputs (list name-temp value-temp)
                         :output dynenv
                         :successor body)))
    (compile-ast client
                 (cleavir-ast:name-ast ast)
                 (clone-context
                  context
                  :result name-temp
                  :successor
                  (compile-ast client
                               (cleavir-ast:value-ast ast)
                               (clone-context
                                context
                                :result value-temp
                                :successor wrapped-body))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile an UNWIND-PROTECT-AST

(defmethod compile-ast (client (ast cleavir-ast:unwind-protect-ast) context)
  (let ((thunk-temp (cleavir-ir:new-temporary))
        (dynenv-out
          (cleavir-ir:make-lexical-location (gensym "unwind-protect"))))
    (compile-ast
     client
     (cleavir-ast:cleanup-thunk-ast ast)
     (clone-context
      context
      :result thunk-temp
      :successor
      (make-instance 'cleavir-ir:unwind-protect-instruction
        :input thunk-temp
        :output dynenv-out
        :successor
        (compile-ast
         client
         (cleavir-ast:protected-form-ast ast)
         (clone-context
          context
          :dynamic-environment-location dynenv-out)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a CALL-AST.

(defmethod compile-ast (client (ast cleavir-ast:call-ast) context)
  (with-accessors ((results results)
                   (successors successors))
      context
    (assert-context ast context nil 1)
    (let* ((all-args (cons (cleavir-ast:callee-ast ast)
                           (cleavir-ast:argument-asts ast)))
           (temps (make-temps all-args))
           ;; In case they diverge at some point.
           (inputs temps))
      (compile-arguments
       client
       all-args
       temps
       (make-instance 'cleavir-ir:funcall-instruction
         :inputs inputs
         :successors
         (if (eq results :values)
             successors
             (list (make-instance 'cleavir-ir:multiple-to-fixed-instruction
                     :outputs results
                     :successor (first successors)))))
       context))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a NAMED-CALL-AST.

(defmethod compile-ast (client (ast cleavir-ast:named-call-ast) context)
  (with-accessors ((results results)
                   (successors successors))
      context
    (assert-context ast context nil 1)
    (let* ((args (cleavir-ast:argument-asts ast))
           (temps (make-temps args))
           (inputs temps))
      (compile-arguments
       client
       args
       temps
       (make-instance 'cleavir-ir:named-call-instruction
         :callee-name (cleavir-ast:callee-name ast)
         :inputs inputs
         :successors
         (if (eq results :values)
             successors
             (list (make-instance 'cleavir-ir:multiple-to-fixed-instruction
                     :outputs results
                     :successor (first successors)))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function COMPILE-FUNCTION
;;;
;;; Returns a new enter instruction and so on for a
;;; FUNCTION-AST. Users that subclass FUNCTION-AST
;;; can define methods on it.

(defgeneric compile-function (client ast))

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
(defmethod compile-function (client (ast cleavir-ast:function-ast))
  (let* ((ll (translate-lambda-list (cleavir-ast:lambda-list ast)))
         (dynenv (cleavir-ir:make-lexical-location (gensym "function")))
         ;; Note the ENTER gets its own output as its dynamic environment.
         (enter (cleavir-ir:make-enter-instruction ll dynenv))
         (return (make-instance 'cleavir-ir:return-instruction
                   :dynamic-environment-location dynenv))
         (body-context (context
                        :values
                        (list return)
                        enter
                        dynenv))
         (body (compile-ast client (cleavir-ast:body-ast ast) body-context)))
    (reinitialize-instance enter :successors (list body))
    enter))

(defvar *function-info*)

(defmethod compile-ast (client (ast cleavir-ast:function-ast) context)
  ;; As per above comment concerning inlining, we memoize here.
  (let ((enter (or (gethash ast *function-info*)
                   (setf (gethash ast *function-info*)
                         (compile-function client ast)))))
    (make-instance 'cleavir-ir:enclose-instruction
     :output (first (results context))
     :successor (first (successors context))
     :code enter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a SETQ-AST.

(defmethod compile-ast (client (ast cleavir-ast:setq-ast) context)
  (let ((location (find-or-create-location (cleavir-ast:lhs-ast ast))))
    (compile-ast
     client
     (cleavir-ast:value-ast ast)
     (clone-context context :result location))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a THE-AST.

(defun make-type-check (type-specifier var successor)
  (make-instance 'cleavir-ir:the-instruction
    :input var
    :outputs '()
    :successor successor
    :value-type type-specifier))

(defmethod compile-ast (client (ast cleavir-ast:the-ast) context)
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
        ((eq results :values)
         (compile-ast client
                      form-ast
                      (clone-context
                       context
                       :successor
                       (make-instance 'cleavir-ir:the-values-instruction
                         :successor successor
                         :required required
                         :optional optional
                         :rest rest))))
        (t ; lexical locations
         (loop for lex in results
               do (setf successor
                        (make-instance 'cleavir-ir:the-instruction
                          :input lex
                          :successor successor
                          :value-type (cond (required (pop required))
                                            (optional (pop optional))
                                            (t rest)))))
         (compile-ast client form-ast (clone-context context :successor successor)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a DYNAMIC-ALLOCATION-AST.

(defmethod compile-ast (client (ast cleavir-ast:dynamic-allocation-ast)
                        context)
  (with-accessors ((results results)
                   (successors successors))
      context
    (assert-context ast context nil 1)
    ;; It's a ONE-VALUE-AST-MIXIN, so RESULTS is one lexical loc.
    (compile-ast
     client
     (cleavir-ast:form-ast ast)
     (clone-context
      context
      :successor
      (make-instance 'cleavir-ir:dynamic-allocation-instruction
        :input (first results)
        :outputs '()
        :successor (first successors))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile an UNREACHABLE-AST.

(defmethod compile-ast (client (ast cleavir-ast:unreachable-ast) context)
  ;; Code like (foo (unreachable)) is possible. In this case
  ;; the context will be expecting a value. But we don't have
  ;; anything to assign. So the location will be uninitialized.
  ;; To avoid any possibility of an instruction using an
  ;; uninitialized location it's better to cut that instruction off
  ;; entirely, i.e. make it not a successor.
  (make-instance 'cleavir-ir:unreachable-instruction))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a SYMBOL-VALUE-AST.

(defmethod compile-ast (client (ast cleavir-ast:symbol-value-ast) context)
  (let ((name-ast (cleavir-ast:name-ast ast)))
    (if (typep name-ast 'cleavir-ast:constant-ast)
        (make-instance 'cleavir-ir:symbol-value-instruction
          :input (make-instance 'cleavir-ir:constant-input
                   :value (cleavir-ast:value name-ast))
          :output (first (results context))
          :successor (first (successors context)))
        (let ((temp (make-temp)))
          (compile-ast client
                       name-ast
                       (clone-context
                        context
                        :result temp
                        :successor  (make-instance 'cleavir-ir:symbol-value-instruction
                                      :input temp
                                      :output (first (results context)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a SET-SYMBOL-VALUE-AST.

(defmethod compile-ast (client (ast cleavir-ast:set-symbol-value-ast) context)
  (let ((name-ast (cleavir-ast:name-ast ast))
        (value-ast (cleavir-ast:value-ast ast)))
    (if (typep name-ast 'cleavir-ast:constant-ast)
        (let ((value-temp (make-temp)))
          (compile-ast
           client
           value-ast
           (clone-context
            context
            :result value-temp
            :successor
            (make-instance 'cleavir-ir:set-symbol-value-instruction
              :inputs (list (make-instance 'cleavir-ir:constant-input
                              :value (cleavir-ast:value name-ast))
                            value-temp)
              :successor (first (successors context))))))
        (let ((variable-temp (make-temp))
              (value-temp (make-temp)))
          (compile-ast
           client
           name-ast
           (clone-context
            context
            :result variable-temp
            :successor
            (compile-ast
             client
             value-ast
             (clone-context
              context
              :result value-temp
              :successor
              (make-instance 'cleavir-ir:set-symbol-value-instruction
                :inputs (list variable-temp value-temp)
                :successor (first (successors context)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FDEFINITION-AST.

(defmethod compile-ast (client (ast cleavir-ast:fdefinition-ast) context)
  (let ((name-ast (cleavir-ast:name-ast ast)))
    (if (typep name-ast 'cleavir-ast:constant-ast)
        (make-instance 'cleavir-ir:fdefinition-instruction
          :input (make-instance 'cleavir-ir:constant-input
                   :value (cleavir-ast:value name-ast))
          :outputs (results context)
          :successors (successors context))
        (let ((temp (make-temp)))
          (compile-ast
           client
           name-ast
           (clone-context
            context
            :result temp
            :successor
            (make-instance 'cleavir-ir:fdefinition-instruction
              :input temp
              :outputs (results context)
              :successors (successors context))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a TYPEQ-AST.

(defmethod compile-ast (client (ast cleavir-ast:typeq-ast) context)
  (with-accessors ((results results)
                   (successors successors))
      context
    (assert-context ast context 0 2)
    (let ((temp (make-temp)))
      (compile-ast
       client
       (cleavir-ast:form-ast ast)
       (clone-context
        context
        :result temp
        :successor  (make-instance 'cleavir-ir:typeq-instruction
                      :input temp
                      :successors successors
                      :value-type (cleavir-ast:type-specifier ast)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a LEXICAL-AST.
;;;
;;; This AST has ONE-VALUE-AST-MIXIN as a superclass.

(defmethod compile-ast (client (ast cleavir-ast:lexical-ast) context)
  (make-instance 'cleavir-ir:assignment-instruction
   :input (find-or-create-location ast)
   :output (first (results context))
   :successor (first (successors context))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COMPILE-TOPLEVEL
;;;
;;; This is the main entry point.

(defun compile-toplevel (client ast)
  (let ((*block-info* (make-hash-table :test #'eq))
        (*go-info* (make-hash-table :test #'eq))
        (*location-info* (make-hash-table :test #'eq))
        (*function-info* (make-hash-table :test #'eq)))
    (check-type ast cleavir-ast:top-level-function-ast)
    (let* ((ll (translate-lambda-list (cleavir-ast:lambda-list ast)))
           (dynenv (cleavir-ir:make-lexical-location (gensym "top")))
           (forms (cleavir-ast:forms ast))
           (enter (cleavir-ir:make-top-level-enter-instruction ll forms dynenv))
           (return (make-instance 'cleavir-ir:return-instruction
                     :dynamic-environment-location dynenv))
           (body-context (context
                          :values
                          (list return)
                          enter
                          dynenv))
           (body (compile-ast client (cleavir-ast:body-ast ast) body-context)))
      ;; Now we must set the successors of the ENTER-INSTRUCTION to a
      ;; list of the result of compiling the AST.
      (reinitialize-instance enter :successors (list body))
      ;; Make sure the list of predecessors of each instruction is
      ;; initialized correctly.
      (cleavir-ir:set-predecessors enter)
      enter)))

(defun compile-toplevel-unhoisted (client ast)
  (let ((*block-info* (make-hash-table :test #'eq))
        (*go-info* (make-hash-table :test #'eq))
        (*location-info* (make-hash-table :test #'eq))
        (*function-info* (make-hash-table :test #'eq)))
    (let* ((ll (translate-lambda-list (cleavir-ast:lambda-list ast)))
           (dynenv (cleavir-ir:make-lexical-location (gensym "topnh")))
           (enter (cleavir-ir:make-enter-instruction ll dynenv))
           (return (make-instance 'cleavir-ir:return-instruction
                     :dynamic-environment-location dynenv))
           (body-context (context
                          :values
                          (list return)
                          enter
                          dynenv))
           (body (compile-ast client (cleavir-ast:body-ast ast) body-context)))
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

(defmethod compile-ast :around (client (ast cleavir-ast:top-level-function-ast) context)
  (declare (ignore context))
  (let* ((enclose (call-next-method))
         (enter (cleavir-ir:code enclose)))
    (change-class enter 'cleavir-ir:top-level-enter-instruction
                  :forms (cleavir-ast:forms ast))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile ASTs that represent low-level operations.

(defun make-temps (arguments &key (class 'cleavir-ir:lexical-location))
  (loop for argument in arguments
        collect (make-temp :class class)))

(defun compile-arguments (client arguments temps successor context)
  (loop with succ = successor
        for arg in (reverse arguments)
        for temp in (reverse temps)
        do (setf succ (compile-ast client
                                   arg
                                   (clone-context
                                    context
                                    :result temp
                                    :successor succ)))
        finally (return succ)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a CONSTANT-AST.
;;;
;;; The CONSTANT-AST is a subclass of ONE-VALUE-AST-MIXIN, so the
;;; :AROUND method on COMPILE-AST has adapted the context so that it
;;; has a single result.

(defmethod compile-ast (client (ast cleavir-ast:constant-ast) context)
  (with-accessors ((results results)
                   (successors successors))
      context
    (make-instance 'cleavir-ir:assignment-instruction
      :input (cleavir-ir:make-constant-input (cleavir-ast:value ast))
      :output (first results)
      :successor (first successors))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a LOAD-CONSTANT-AST.
;;;
;;; The LOAD-CONSTANT-AST is a subclass of ONE-VALUE-AST-MIXIN, so the
;;; :AROUND method on COMPILE-AST has adapted the context so that it
;;; has a single result.

(defmethod compile-ast (client (ast cleavir-ast:load-constant-ast) context)
  (with-accessors ((results results)
                   (successors successors))
      context
    (make-instance 'cleavir-ir:load-constant-instruction
      :location-info (cleavir-ast:location-info ast)
      :inputs '()
      :output (first results)
      :successor (first successors))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a LOAD-LITERAL-AST.
;;;
;;; The LOAD-LITERAL-AST is a subclass of ONE-VALUE-AST-MIXIN, so the
;;; :AROUND method on COMPILE-AST has adapted the context so that it
;;; has a single result.

(defmethod compile-ast (client (ast cleavir-ast:load-literal-ast) context)
  (with-accessors ((results results)
                   (successors successors))
      context
    (make-instance 'cleavir-ir:load-literal-instruction
      :location-info (cleavir-ast:location-info ast)
      :inputs '()
      :output (first results)
      :successor (first successors))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a IMMEDIATE-AST.
;;;
;;; The IMMEDIATE-AST is a subclass of ONE-VALUE-AST-MIXIN, so the
;;; :AROUND method on COMPILE-AST has adapted the context so that it
;;; has a single result.

(defmethod compile-ast (client (ast cleavir-ast:immediate-ast) context)
  (with-accessors ((results results)
                   (successors successors))
      context
    (make-instance 'cleavir-ir:assignment-instruction
      :input (cleavir-ir:make-immediate-input (cleavir-ast:value ast))
      :output (first results)
      :successor (first successors))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile an EQ-AST.

(defmethod compile-ast (client (ast cleavir-ast:eq-ast) context)
  (with-accessors ((successors successors))
      context
    (ecase (length successors)
      (1
       (compile-ast client
                    (make-instance 'cleavir-ast:if-ast
                      :test-ast ast
                      :then-ast (make-instance 'cleavir-ast:constant-ast
                                  :value (list 'quote t))
                      :else-ast (make-instance 'cleavir-ast:constant-ast
                                  :value (list 'quote nil)))
                    context))
      (2
       (assert-context ast context 0 2)
       (let ((arg1-ast (cleavir-ast:arg1-ast ast))
             (arg2-ast (cleavir-ast:arg2-ast ast))
             (temp1 (cleavir-ir:new-temporary))
             (temp2 (cleavir-ir:new-temporary)))
         (if (eq (first successors)
                 (second successors))
             ;; This means we don't really need the test
             (compile-ast
              client
              arg1-ast
              (clone-context
               context
               :results '()
               :successor
               (compile-ast
                client
                arg2-ast
                (clone-context
                 context
                 :results '()
                 :successor (first successors)))))
             (compile-ast
              client
              arg1-ast
              (clone-context
               context
               :result temp1
               :successor
               (compile-ast
                client
                arg2-ast
                (clone-context
                 context
                 :result temp2
                 :successor (make-instance 'cleavir-ir:eq-instruction
                              :inputs (list temp1 temp2)
                              :successors successors)))))))))))
