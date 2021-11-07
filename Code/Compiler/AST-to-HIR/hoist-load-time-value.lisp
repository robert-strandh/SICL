(cl:in-package #:sicl-ast-to-hir)

;;;; LOAD-TIME-VALUE-ASTs are handled as follows.  We separate the
;;;; description into one part that describes how it works in the
;;;; intrinsic (native) loader, and another part that describes how it
;;;; works in the extrinsic (cross-) loader.  The difference between
;;;; the two is that the code executed at load time is done by native
;;;; processor instructions in the intrinsic loader, and by the HIR
;;;; evaluator in the extrinsic loader.  Common between the two cases
;;;; is that the LOAD-TIME-VALUE-AST is turned into a LOAD-LITERAL-AST
;;;; and the FORM-AST of the LOAD-TIME-VALUE-AST is moved to the top
;;;; level for evaluation at load time. The FORM-AST becomes the
;;;; argument of a PATCH-LITERAL-AST that is connected to the
;;;; LOAD-LITERAL-AST.  Both the LOAD-LITERAL-AST and the
;;;; PATCH-LITERAL-AST have corresponding HIR instructions
;;;; LOAD-LITERAL-INSTRUCTION and PATCH-LITERAL-INSTRUCTION.
;;;; Typically, the LOAD-LITERAL-INSTRUCTION is not executed at load
;;;; time, but it is entirely possible for load-time code to contain a
;;;; LOAD-TIME-VALUE form, so that the LOAD-LITERAL-INSTRUCTION is
;;;; also executed at load time.  Either way, it is processed by the
;;;; code generator and turned into one or more native instructions
;;;; that, when executed, construct a word in a register with a value
;;;; corresponding to the object.  If the object is heap-allocated,
;;;; then it is promoted to the global heap so that it remains a
;;;; constant value at run time.  If the processor has an instruction
;;;; that can load a full word from the instruction stream, then the
;;;; object simply becomes the value of that word.  In most RISC
;;;; processors, no such instruction exists, and then the object will
;;;; have to be constructed from two or more parts, each parts
;;;; supplied by a separate instruction.  Either way, the instruction
;;;; stream will have to be patched once the object has been created
;;;; by the load-time code, and that is the role of the
;;;; PATCH-LITERAL-AST and the PATCH-LITERAL-INSTRUCTION.

;;;; In the intrinsic loader, the PATCH-LITERAL-INSTRUCTION is turned
;;;; into one or more memory-write instructions by the code generator.
;;;; The purpose is to write the constructed object into the
;;;; instruction stream where the instructions resulting from the
;;;; LOAD-LITERAL-INSTRUCTION are located.  The constructed object
;;;; will be present in a register, so the memory-write instructions
;;;; write all or part of this register to the addresses where the
;;;; instructions resulting from the LOAD-LITERAL-INSTRUCTION are
;;;; located.  For that to work, the addresses of those instructions
;;;; must be stored in the memory-write instructions.  But these
;;;; addresses are not known until code has been fully generated for
;;;; the entire compilation unit.  So the memory-write instructions
;;;; must themselves be patched by the loader, once the code generator
;;;; has fully processed the code.  Furthermore, the loader must keep
;;;; track of the addresses of the instructions resulting from the
;;;; LOAD-LITERAL-INSTRUCTION throughout the code-generation phase.

;;;; In the extrinsic loader, the PATCH-LITERAL-INSTRUCTION is
;;;; executed by the HIR evaluator after native code for the entire
;;;; compilation unit has been generated.  The associated
;;;; LOAD-LITERAL-INSTRUCTION might be executed by the HIR evaluator
;;;; later during the execution of load-time code, or it may just
;;;; become part of the native code to be executed at run time.  Both
;;;; possibilities must be taken into account.  For the first case, a
;;;; CONS cell is shared between the LOAD-LITERAL-AST and the
;;;; PATCH-LITERAL-AST, and this CONS cell is transmitted to the
;;;; corresponding HIR instructions.  When the HIR evaluator executes
;;;; the PATCH-LITERAL-INSTRUCTION, it stores the constructed object
;;;; in the CAR of the CONS cell.  When the HIR evaluator executes the
;;;; LOAD-LITERAL-INSTRUCTION, it reads the CAR of the CONS cell to
;;;; obtain the object.  For the second case, the
;;;; PATCH-LITERAL-INSTRUCTION contains a slot that holds the
;;;; addresses in the native instruction stream that must be patched.
;;;; These addresses are known only after code generation is complete,
;;;; so the extrinsic loader must keep track of those addresses and
;;;; store them in the PATCH-LITERAL-INSTRUCTION before the load-time
;;;; code can be executed by the HIR evaluator.  Furthermore, if the
;;;; object created by the load-time code is a heap-allocated object,
;;;; the HIR evaluator must turn it into a native address by
;;;; allocating space for it in the simulated global heap.

;;; Since we map in DEPTH-FIRST PRE-ORDER we accumulate the outermost
;;; ASTs first, but since we then PUSH them to a list, the list ends
;;; up having the innermost ASTs first.
(defun find-load-time-value-asts (ast)
  (let ((result '()))
    (cleavir-ast:map-ast-depth-first-preorder
     (lambda (node)
       (when (typep node 'cleavir-ast:load-time-value-ast)
         (push node result)))
     ast)
    result))

(defun split-load-time-value-ast-list (load-time-value-asts)
  (assert (not (null load-time-value-asts)))
  (loop with first = (first load-time-value-asts)
        with first-form-ast = (cleavir-ast:form-ast first)
        for ast in (rest load-time-value-asts)
        if (eq (cleavir-ast:form-ast ast) first-form-ast)
          collect ast into yes
        else
          collect ast into no
        finally (return (values (cons first yes) no))))

(defun group-load-time-value-asts (load-time-value-asts)
  (loop with remaining = load-time-value-asts
        until (null remaining)
        collect (multiple-value-bind (yes no)
                    (split-load-time-value-ast-list remaining)
                  (setf remaining no)
                  yes)))

(defun process-one-load-time-value-ast (load-time-value-ast lexical-ast)
  (let* ((code-vector-index-ast
           (make-instance 'cleavir-ast:literal-ast :value 0))
         (literals-vector-index-ast
           (make-instance 'cleavir-ast:literal-ast :value 0))
         (literal-cell (list nil)))
    (change-class load-time-value-ast 'cleavir-ast:load-literal-ast
                  :location-info literal-cell)
    (make-instance 'sicl-ast:patch-literal-ast
      :literal-cell literal-cell
      :literal-ast lexical-ast
      :code-vector-index-ast code-vector-index-ast
      :literals-vector-index-ast literals-vector-index-ast)))

(defun process-one-load-time-value-ast-group (group lexical-ast)
  (loop for load-time-value-ast in group
        collect
        (process-one-load-time-value-ast load-time-value-ast lexical-ast)))

;;; Since we want the innermost LOAD-TIME-VALUEs to be executed first,
;;; we must push them in the order of the outermost first.  For that
;;; reason, we REVERSE the list before processing it.
(defun hoist-load-time-value (ast)
  (let* ((load-time-value-asts (find-load-time-value-asts ast))
         (groups (group-load-time-value-asts load-time-value-asts))
         (form-asts (list ast)))
    (loop for count from 0
          for group in (reverse groups)
          for form-ast = (cleavir-ast:form-ast (first group))
          for lexical-ast = (cleavir-ast:make-ast 'cleavir-ast:lexical-ast
                              :name (gensym))
          for bind-ast = (cleavir-ast:make-ast 'cleavir-ast:lexical-bind-ast
                           :lexical-variable-ast lexical-ast
                           :value-ast form-ast)
          for patch-literal-asts
            = (process-one-load-time-value-ast-group group lexical-ast)
          do (setf form-asts (append patch-literal-asts form-asts))
             (push bind-ast form-asts)
          finally (return (values (make-instance 'cleavir-ast:progn-ast
                                    :form-asts form-asts)
                                  count)))))
