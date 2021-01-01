(cl:in-package #:cleavir-value-hoisting)

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
   (make-instance 'cleavir-ir:assignment-instruction
     :input input
     :output output)))

(defun invoke-thunk-at-load-time (thunk &optional (outputs '()))
  (let ((nop (make-instance 'cleavir-ir:nop-instruction))
        (enclosed-thunk (cleavir-ir:new-temporary)))
    (emit-instruction (make-instance 'cleavir-ir:enclose-instruction
                        :output enclosed-thunk
                        :successor nop
                        :code thunk))
    (emit-instruction (make-instance 'cleavir-ir:funcall-instruction
                        :inputs (list enclosed-thunk)))
    (emit-instruction (make-instance 'cleavir-ir:multiple-to-fixed-instruction
                        :outputs outputs
                        :successor nop))))

;;; Ensure that CONSTRUCTOR is run at load time and that the resulting
;;; object is stored in the lexical variable OUTPUT.
(defun reconstruct (client constructor output)
  (with-accessors ((creation-thunk creation-thunk)
                   (initialization-thunk initialization-thunk)
                   (lexical-location lexical-location)) constructor
    (if lexical-location
        (assign-at-load-time lexical-location output)
        (progn
          (hoist-hir client creation-thunk)
          (invoke-thunk-at-load-time creation-thunk (list output))
          (setf lexical-location output)
          (unless (null initialization-thunk)
            (invoke-thunk-at-load-time initialization-thunk)
            (hoist-hir client initialization-thunk))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Hoisting

(defmethod hoist-toplevel-hir (client (hir cleavir-ir:enter-instruction))
  (let ((*current-instruction* hir))
    (hoist-hir client hir)
    hir))

(defmethod hoist-hir (client (hir cleavir-ir:instruction))
  (cleavir-ir:map-instructions
   (lambda (instruction)
     (loop for datum in (cleavir-ir:inputs instruction) do
       (hoist-datum client datum))
     instruction)
   hir))

(defmethod hoist-datum (client (datum cleavir-ir:datum))
  (values))

(defmethod hoist-datum (client (constant-input cleavir-ir:constant-input))
  (let ((value (cleavir-ir:value constant-input)))
    (reconstruct
     client
     (constructor value)
     (change-class constant-input 'cleavir-ir:lexical-location
       :name (cleavir-ir:value constant-input)))))
