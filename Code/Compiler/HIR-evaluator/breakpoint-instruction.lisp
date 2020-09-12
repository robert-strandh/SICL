(cl:in-package #:sicl-hir-evaluator)

(defparameter *table* (make-hash-table :test #'eq))

(defmethod instruction-thunk
    (client
     (instruction sicl-ir:breakpoint-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment)
    ;; (clordane::show (sicl-compiler:debug-information instruction))
    ;; (clordane::wait)
    ;; Uncomment this code in order to get an idea of what
    ;; code is executed by the HIR evaluator.
    ;; (let ((info (sicl-ir:debug-information instruction)))
    ;;  (unless (null info)
    ;;    (incf (gethash info *table* 0))))
    (successor 0)))
