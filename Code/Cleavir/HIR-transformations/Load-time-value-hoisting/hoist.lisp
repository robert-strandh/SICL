(cl:in-package #:cleavir-load-time-value-hoisting)

;;; Hoisting takes place after scanning.  During hoisting, all occurring
;;; data are processed by HOIST-DATUM.  The default methods replace each
;;; constant input and each load time value input by a lexical locations,
;;; and arrange that this lexical locations is properly initialized at load
;;; time by calling the suitable creation and initialization thunk.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Auxiliary Functions

;;; The initial instruction is the instruction that, is the successor of
;;; the enter instruction passed to HOIST-TOPLEVEL-HIR.  All calls to
;;; creation and initialization thunks are placed before this instruction.
(defvar *toplevel-enter-instruction*)

(defun push-instruction (instruction)
  (cleavir-ir:insert-instruction-before instruction *initial-instruction*))

(defun push-thunk-invocation (thunk &optional (outputs '()))
  (unless (not thunk)
    (let* ((before-instruction *toplevel-enter-instruction*)
           (after-instruction
             (first
              (cleavir-ir:successors before-instruction)))
           (enclosed-thunk
             (cleavir-ir:new-temporary))
           (funcall-result
             (cleavir-ir:make-values-location))
           (successor
             (cleavir-ir:make-multiple-to-fixed-instruction
              funcall-result outputs after-instruction))
           (successor
             (cleavir-ir:make-funcall-instruction
              (list enclosed-thunk)
              (list funcall-result)
              successor))
           (successor
             (cleavir-ir:make-enclose-instruction
              enclosed-thunk successor thunk)))
      (setf (cleavir-ir:successors before-instruction)
            (list successor)))))

;;; Ensure that CONSTRUCTOR is run at load time and that the resulting
;;; object is stored in the lexical variable OUTPUT.
(defun reconstruct (constructor output system)
  (with-accessors ((creation-thunk creation-thunk)
                   (initialization-thunk initialization-thunk)
                   (lexical-location lexical-location)) constructor
    (if lexical-location
        (push-instruction
         (cleavir-ir:make-assignment-instruction lexical-location output))
        (let ((output (cleavir-ir:new-temporary)))
          (hoist-hir creation-thunk system)
          (push-thunk-invocation creation-thunk (list output))
          (setf lexical-location output)
          (hoist-hir initialization-thunk system)
          (push-thunk-invocation initialization-thunk)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Hoisting

(defmethod hoist-toplevel-hir ((hir cleavir-ir:enter-instruction) system)
  (let ((*toplevel-enter-instruction* hir)
        (cleavir-ir:*policy* (cleavir-ir:policy hir)))
    (hoist-hir hir system)
    hir))

(defmethod hoist-hir ((hir null) system)
  (values))

(defmethod hoist-hir ((hir cleavir-ir:instruction) system)
  (cleavir-ir:map-instructions
   (lambda (instruction)
     (loop for datum in (cleavir-ir:inputs instruction) do
       (hoist-datum datum system))
     instruction)
   hir))

(defmethod hoist-datum ((datum cleavir-ir:datum) system)
  (values))

(defmethod hoist-datum ((load-time-value-input cleavir-ir:load-time-value-input) system)
  (reconstruct (constructor load-time-value-input)
               (change-class load-time-value-input 'cleavir-ir:lexical-location
                 :name (cleavir-ir:form load-time-value-input))
               system))

(defmethod hoist-datum ((constant-input cleavir-ir:constant-input) system)
  (let ((value (cleavir-ir:value constant-input)))
    (reconstruct (constructor value)
                 (change-class constant-input 'cleavir-ir:lexical-location
                   :name (cleavir-ir:value constant-input))
                 system)))
