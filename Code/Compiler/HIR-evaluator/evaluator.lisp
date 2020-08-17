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
(defvar *instruction-thunks* nil)

;; The main entry point for converting instructions to thunks.
(defgeneric instruction-thunk (client instruction lexical-environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Representing the Dynamic and Lexical Environment
;;;
;;; The lexical environment is represented as a hash table from HIR inputs
;;; (and, occasionally, symbols) to cons cells whose CAR holds the value.
;;;
;;; The dynamic environment is represented as a linked list of entries.

(defun value-cell (name lexical-environment)
  (multiple-value-bind (cell present-p)
      (gethash name lexical-environment)
    (if present-p
        cell
        (setf (gethash name lexical-environment)
              (if (typep name 'cleavir-ir:constant-input)
                  (list (cleavir-ir:value name))
                  (list nil))))))

(defmethod instruction-thunk :around
    (client instruction lexical-environment)
  (multiple-value-bind (thunk presentp)
      (gethash instruction *instruction-thunks*)
    (if presentp thunk (call-next-method))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Representing HIR as host functions.

(defun hir-to-host-function (client enter-instruction lexical-environment)
  (let* ((*instruction-thunks* (make-hash-table :test #'eq))
         (static-environment-cell-1
           (value-cell 'static-environment lexical-environment))
         (static-environment-cell-2
           (value-cell (cleavir-ir:static-environment enter-instruction) lexical-environment))
         (dynamic-environment-cell-1
           (value-cell 'dynamic-environment lexical-environment))
         (dynamic-environment-cell-2
           (value-cell (cleavir-ir:dynamic-environment-output enter-instruction) lexical-environment))
         (arguments-cell
           (value-cell 'arguments lexical-environment))
         (successor
           (first (cleavir-ir:successors enter-instruction)))
         (thunk
           (instruction-thunk client successor lexical-environment)))
    (lambda (arguments static-environment dynamic-environment)
      (setf (car static-environment-cell-1) static-environment)
      (setf (car static-environment-cell-2) static-environment)
      (setf (car dynamic-environment-cell-1) dynamic-environment)
      (setf (car dynamic-environment-cell-2) dynamic-environment)
      (setf (car arguments-cell) (coerce arguments 'vector))
      (let ((thunk thunk))
        (catch 'return
          (loop (setf thunk (funcall thunk))))))))

(defun top-level-hir-to-host-function (client enter-instruction)
  (let* ((*instruction-thunks* (make-hash-table :test #'eq))
         (lexical-environment (make-hash-table :test #'eq))
         (static-environment-cell-1
           (value-cell 'static-environment lexical-environment))
         (static-environment-cell-2
           (value-cell (cleavir-ir:static-environment enter-instruction) lexical-environment))
         (dynamic-environment-cell-1
           (value-cell 'dynamic-environment lexical-environment))
         (dynamic-environment-cell-2
           (value-cell (cleavir-ir:dynamic-environment-output enter-instruction) lexical-environment))
         (successor
           (first (cleavir-ir:successors enter-instruction)))
         (thunk
           (instruction-thunk client successor lexical-environment)))
    (lambda (static-environment)
      (setf (car static-environment-cell-1) static-environment)
      (setf (car static-environment-cell-2) static-environment)
      (setf (car dynamic-environment-cell-1) '())
      (setf (car dynamic-environment-cell-2) '())
      (let ((thunk thunk))
        (catch 'return
          (loop (setf thunk (funcall thunk))))))))
