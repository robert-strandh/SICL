(cl:in-package #:cleavir-load-time-value-hoisting)

;;; Hoisting takes place after scanning.  During hoisting, all occurring
;;; data are processed by HOIST-DATUM.  The default methods replace all
;;; constant inputs and load time value inputs by lexical locations, and
;;; arrange that each of these lexical locations is properly initialized by
;;; the suitable creation and initialization thunk.  This sequence of
;;; thunks is placed right after the enter instruction that is passed as a
;;; first argument to HOIST-TOPLEVEL-HIR.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Auxiliary Functions

;;; The initial instruction is the instruction that, initially, is the
;;; successor of the enter instruction passed to HOIST-TOPLEVEL-HIR.  All
;;; calls to creation and initialization thunks are placed before this
;;; instruction.
(defvar *initial-instruction*)

(defun push-instruction (instruction)
  (cleavir-ir:insert-instruction-before instruction *initial-instruction*))

(defun push-thunk-invocation (thunk &optional outputs)
  (let ((fvar (cleavir-ir:new-temporary))
        (values (cleavir-ir:make-values-location)))
    (push-instruction
     (cleavir-ir:make-enclose-instruction fvar nil thunk))
    (push-instruction
     (cleavir-ir:make-funcall-instruction (list fvar) (list values)))
    (push-instruction
     (cleavir-ir:make-multiple-to-fixed-instruction values outputs))))

;;; Ensure that OBJECT is suitably created and initialized at load time and
;;; that the resulting similar object is stored in the lexical variable
;;; OUTPUT.
(defun reconstruct (object output system)
  (with-accessors ((creation-thunk creation-thunk)
                   (initialization-thunk initialization-thunk)
                   (lexical-location lexical-location))
      (constructor object)
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
  (let ((*initial-instruction* (first (cleavir-ir:successors hir))))
    (hoist-hir hir system)
    hir))

(defmethod hoist-hir ((hir null) system)
  (values))

(defmethod hoist-hir ((hir cleavir-ir:instruction) system)
  (cleavir-ir:map-instructions
   (lambda (instruction)
     (loop for datum in (cleavir-ir:inputs instruction)
           unless (immediate-p datum system) do
       (hoist-datum datum system))
     instruction)
   hir))

(defmethod hoist-datum ((immediate-input cleavir-ir:immediate-input) system)
  (values))

(defmethod hoist-datum ((load-time-value-input cleavir-ir:load-time-value-input) system)
  (reconstruct load-time-value-input
               (change-class load-time-value-input 'cleavir-ir:lexical-location)
               system))

(defmethod hoist-datum ((constant-input cleavir-ir:constant-input) system)
  (let ((value (cleavir-ir:value constant-input)))
    (unless (immedate-p value system)
      (reconstruct value
                   (change-class constant-input 'cleavir-ir:lexical-location)
                   system))))
