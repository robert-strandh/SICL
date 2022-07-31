(cl:in-package #:sicl-ir)

(defclass top-level-enter-instruction (cleavir-ir:enter-instruction)
  ((%literals :initarg :literals :accessor literals)
   (%call-sites :initarg :call-sites :accessor call-sites)))

(defmethod cleavir-ir:clone-initargs append
    ((instruction top-level-enter-instruction))
  (list :literals (literals instruction)
        :call-sites (call-sites instruction)))
