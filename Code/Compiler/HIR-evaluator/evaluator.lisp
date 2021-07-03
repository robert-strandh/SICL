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

(defparameter *instruction-thunk-meter*
  (make-instance 'cleavir-meter:basic-meter
    :name "INSTRUCTION-THUNK"))

(defmethod instruction-thunk :around
    (client instruction lexical-environment)
  (multiple-value-bind (thunk presentp)
      (gethash instruction *instruction-thunks*)
    (if presentp thunk (call-next-method))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Representing HIR as host functions.

(defun hir-to-host-function (client enter-instruction lexical-environment)
  (let* ((lexical-environment (make-lexical-environment))
         (static-environment-lref-1
           (ensure-lref 'static-environment lexical-environment))
         (static-environment-lref-2
           (ensure-lref (cleavir-ir:static-environment enter-instruction) lexical-environment))
         (dynamic-environment-lref-1
           (ensure-lref 'dynamic-environment lexical-environment))
         (dynamic-environment-lref-2
           (ensure-lref (cleavir-ir:dynamic-environment-output enter-instruction) lexical-environment))
         (arguments-lref
           (ensure-lref 'arguments lexical-environment))
         (successor
           (cleavir-ir:first-successor enter-instruction))
         (thunk
           (cleavir-meter:with-meter (meter *instruction-thunk-meter*)
             (instruction-thunk client successor lexical-environment))))
    (lambda (arguments static-environment dynamic-environment)
      (let ((lexical-locations (lexical-environment-vector lexical-environment))
            (thunk thunk))
        (macrolet ((lref (lref)
                     `(%lref lexical-locations ,lref)))
          (setf (lref static-environment-lref-1) static-environment)
          (setf (lref static-environment-lref-2) static-environment)
          (setf (lref dynamic-environment-lref-1) dynamic-environment)
          (setf (lref dynamic-environment-lref-2) dynamic-environment)
          (setf (lref arguments-lref) (coerce arguments 'vector)))
        (catch 'return
          (loop (setf thunk (funcall thunk lexical-locations))))))))

;;; The top-level HIR function is not subject to argument processing,
;;; because we completely control how it is called.  Instead, we just
;;; pass it a single required parameter, namely a vector to be filled
;;; in with values of LOAD-TIME-VALUE forms and constants created by
;;; the reader.  The parameter LEXICAL-LOCATION is the third output of
;;; the ENTER-INSTRUCTION.  (FIXME: should we access it as the first
;;; element of the lambda list instead?)

(defun top-level-hir-to-host-function (client enter-instruction)
  (let* ((*instruction-thunks* (make-hash-table :test #'eq))
         (lexical-environment (make-lexical-environment))
         (static-environment-lref-1
           (ensure-lref 'static-environment lexical-environment))
         (static-environment-lref-2
           (ensure-lref (cleavir-ir:static-environment enter-instruction) lexical-environment))
         (dynamic-environment-lref-1
           (ensure-lref 'dynamic-environment lexical-environment))
         (dynamic-environment-lref-2
           (ensure-lref (cleavir-ir:dynamic-environment-output enter-instruction) lexical-environment))
         (arguments-lref
           (ensure-lref 'arguments lexical-environment))
         (argument-lref
           (ensure-lref (third (cleavir-ir:outputs enter-instruction)) lexical-environment))
         (successor
           (cleavir-ir:first-successor enter-instruction))
         (thunk
           (cleavir-meter:with-meter (meter *instruction-thunk-meter*)
             (instruction-thunk client successor lexical-environment))))
    (lambda (constants-vector static-environment)
      (let ((lexical-locations (lexical-environment-vector lexical-environment))
            (thunk thunk))
        (macrolet ((lref (lref)
                     `(%lref lexical-locations ,lref)))
          (setf (lref static-environment-lref-1) static-environment)
          (setf (lref static-environment-lref-2) static-environment)
          (setf (lref dynamic-environment-lref-1) '())
          (setf (lref dynamic-environment-lref-2) '())
          (setf (lref arguments-lref) (vector constants-vector))
          (setf (lref argument-lref) constants-vector))
        (catch 'return
          (loop (setf thunk (funcall thunk lexical-locations))))))))
