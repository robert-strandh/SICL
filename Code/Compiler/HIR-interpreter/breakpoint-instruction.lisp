(cl:in-package #:sicl-hir-interpreter)

(defmethod interpret-instruction
    (client
     (instruction sicl-ast-to-hir:breakpoint-instruction)
     lexical-environment)
  (declare (ignore client lexical-environment))
  (clordane::show (sicl-ast-to-hir:debug-information instruction))
  (clordane::wait)
  (first (cleavir-ir:successors instruction)))
