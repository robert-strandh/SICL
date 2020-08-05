(in-package #:sicl-hir-evaluator)

;; A list of call stack entries.
(defparameter *call-stack* '())

(defclass call-stack-entry ()
  ((%origin :initarg :origin :reader origin)
   (%arguments :initarg :arguments :reader arguments)))

;; A list of all values returned by the last function call.
(defvar *global-values-location*)

;; A hash table, caching the thunk of each instruction that has already
;; been converted.
(defvar *instruction-thunks*)

;; The main entry point for converting instructions to thunks.
(defgeneric instruction-thunk (client instruction lexical-environment))

(defun evaluate-hir (client enter-instruction lexical-environment)
  (let ((thunk
          (let ((*instruction-thunks* (make-hash-table)))
            (instruction-thunk
             client
             (first (cleavir-ir:successors enter-instruction))
             lexical-environment))))
    (loop (setf thunk (funcall thunk)))))

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

;;; I haven't fully grasped this method, yet. - MH
(defmethod instruction-thunk :around
    (client instruction lexical-environment)
  (multiple-value-bind (thunk presentp)
      (gethash instruction *instruction-thunks*)
    (if presentp
        thunk
        (let ((thunk (call-next-method))
              (environment-cell-1
                (value-cell 'dynamic-environment lexical-environment))
              (environment-cell-2
                (value-cell (cleavir-ir:dynamic-environment-location instruction)
                            lexical-environment)))
          (assert (functionp thunk))
          (lambda ()
            (let ((env1 (car environment-cell-1))
                  (env2 (car environment-cell-2)))
              (prog1 (if (or (eq env1 env2)
                             (> (length env2) (length env1)))
                         (funcall thunk)
                         (progn
                           ;; Invalidate entries.
                           (loop for env = env1 then (rest env)
                                 for entry = (first env)
                                 until (eq env env2)
                                 do (sicl-run-time:invalidate-entry entry))
                           ;; ?
                           (let ((last-block/tagbody
                                   (loop with result = nil
                                         for env = env1 then (rest env)
                                         for entry = (first env)
                                         until (eq env env2)
                                         when (typep entry 'sicl-run-time:unwind-protect-entry)
                                           do (funcall (sicl-run-time:thunk entry))
                                         when (typep entry 'sicl-run-time:block/tagbody-entry)
                                           do (setf result entry)
                                         finally (return result))))
                             (unless (null last-block/tagbody)
                               (throw (sicl-run-time:frame-pointer last-block/tagbody)
                                 thunk)))
                           (funcall thunk)))
                (setf (car environment-cell-1)
                      (car environment-cell-2)))))))))
