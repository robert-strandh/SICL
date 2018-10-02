(cl:in-package #:cleavir-load-time-value-hoisting)

;;; Hoisting takes place after scanning.  During hoisting, all occurring
;;; data are processed by HOIST-DATUM.  The default methods replace each
;;; constant input and each load time value input by a lexical locations,
;;; and arrange that this lexical locations is properly initialized at load
;;; time by calling the suitable creation and initialization thunk.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Auxiliary Functions

(defvar *current-instruction*)

(defun emit-instruction (instruction)
  (cleavir-ir:insert-instruction-after instruction *current-instruction*)
  (setf *current-instruction* instruction)
  (values))

(defun assign-at-load-time (input output)
  (emit-instruction
   (cleavir-ir:make-assignment-instruction input output)))

(defun invoke-thunk-at-load-time (thunk &optional (outputs '()))
  (let ((nop (cleavir-ir:make-nop-instruction '()))
        (enclosed-thunk (cleavir-ir:new-temporary))
        (funcall-result (cleavir-ir:make-values-location)))
    (emit-instruction (cleavir-ir:make-enclose-instruction
                       enclosed-thunk nop thunk))
    (emit-instruction (cleavir-ir:make-funcall-instruction
                       (list enclosed-thunk)
                       (list funcall-result)))
    (emit-instruction (cleavir-ir:make-multiple-to-fixed-instruction
                       funcall-result
                       outputs
                       nop))))

;;; Ensure that CONSTRUCTOR is run at load time and that the resulting
;;; object is stored in the lexical variable OUTPUT.
(defun reconstruct (constructor output system)
  (with-accessors ((creation-thunk creation-thunk)
                   (initialization-thunk initialization-thunk)
                   (lexical-location lexical-location)) constructor
    (if lexical-location
        (assign-at-load-time lexical-location output)
        (progn
          (hoist-hir creation-thunk system)
          (invoke-thunk-at-load-time creation-thunk (list output))
          (setf lexical-location output)
          (unless (null initialization-thunk)
            (invoke-thunk-at-load-time initialization-thunk)
            (hoist-hir initialization-thunk system))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Hoisting

(defmethod hoist-toplevel-hir ((hir cleavir-ir:enter-instruction) system)
  (let ((*current-instruction* hir)
        (cleavir-ir:*policy* (cleavir-ir:policy hir)))
    (hoist-hir hir system)
    hir))

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
