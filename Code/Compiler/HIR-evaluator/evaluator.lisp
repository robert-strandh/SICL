(in-package #:sicl-hir-evaluator)

;; A list of call stack entries.
(defparameter *call-stack* '())

(defclass call-stack-entry ()
  ((%origin :initarg :origin :reader origin)
   (%arguments :initarg :arguments :reader arguments)))

;; A list of all values returned by the last function call.
(defvar *global-values-location* nil)

;; A hash table, caching the thunk of each instruction that has already
;; been converted.
(defvar *instruction-thunks*)

;; The main entry point for converting instructions to thunks.
(defgeneric instruction-thunk (client instruction lexical-environment))

(defmethod instruction-thunk :around
    (client instruction lexical-environment)
  (multiple-value-bind (thunk presentp)
      (gethash instruction *instruction-thunks*)
    (if presentp thunk (call-next-method))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Representing HIR as host functions.

(defun hir-to-host-function (client enter-instruction lexical-environment)
  (let* ((static-environment-index-1
           (value-index 'static-environment lexical-environment))
         (static-environment-index-2
           (value-index (cleavir-ir:static-environment enter-instruction) lexical-environment))
         (dynamic-environment-index-1
           (value-index 'dynamic-environment lexical-environment))
         (dynamic-environment-index-2
           (value-index (cleavir-ir:dynamic-environment-output enter-instruction) lexical-environment))
         (arguments-index
           (value-index 'arguments lexical-environment))
         (successor
           (first (cleavir-ir:successors enter-instruction)))
         (thunk
           (instruction-thunk client successor lexical-environment)))
    (lambda (arguments static-environment dynamic-environment)
      (let ((lexical-locations (lexical-environment-vector lexical-environment))
            (thunk thunk))
        (macrolet ((lref (index)
                     `(svref lexical-locations ,index)))
          (setf (lref static-environment-index-1) static-environment)
          (setf (lref static-environment-index-2) static-environment)
          (setf (lref dynamic-environment-index-1) dynamic-environment)
          (setf (lref dynamic-environment-index-2) dynamic-environment)
          (setf (lref arguments-index) (coerce arguments 'vector)))
        (catch 'return
          (loop (setf thunk (funcall thunk lexical-locations))))))))

(defun top-level-hir-to-host-function (client enter-instruction)
  (let* ((*instruction-thunks* (make-hash-table :test #'eq))
         (lexical-environment (make-instance 'lexical-environment))
         (static-environment-index-1
           (value-index 'static-environment lexical-environment))
         (static-environment-index-2
           (value-index (cleavir-ir:static-environment enter-instruction) lexical-environment))
         (dynamic-environment-index-1
           (value-index 'dynamic-environment lexical-environment))
         (dynamic-environment-index-2
           (value-index (cleavir-ir:dynamic-environment-output enter-instruction) lexical-environment))
         (successor
           (first (cleavir-ir:successors enter-instruction)))
         (thunk
           (instruction-thunk client successor lexical-environment)))
    (lambda (static-environment)
      (let ((lexical-locations (lexical-environment-vector lexical-environment))
            (thunk thunk))
        (macrolet ((lref (index)
                     `(svref lexical-locations ,index)))
          (setf (lref static-environment-index-1) static-environment)
          (setf (lref static-environment-index-2) static-environment)
          (setf (lref dynamic-environment-index-1) '())
          (setf (lref dynamic-environment-index-2) '()))
        (catch 'return
          (loop (setf thunk (funcall thunk lexical-locations))))))))
