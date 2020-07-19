(cl:in-package #:sicl-hir-evaluator)

(defmethod instruction-thunk
    (client
     (instruction sicl-ir:breakpoint-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment)
    ;; (clordane::show (sicl-compiler:debug-information instruction))
    ;; (clordane::wait)
    (successor 0)))
