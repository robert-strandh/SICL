(cl:in-package #:sicl-hir-interpreter)

(defmethod interpret-instruction
    (client
     (instruction sicl-ir:breakpoint-instruction)
     lexical-environment)
  (declare (ignore client lexical-environment))
  ;; (clordane::show (sicl-compiler:debug-information instruction))
  ;; (clordane::wait)
  (first (cleavir-ir:successors instruction)))
