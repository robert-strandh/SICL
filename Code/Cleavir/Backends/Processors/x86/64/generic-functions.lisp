(cl:in-package #:cleavir-processor-x86-64)

(defgeneric implementation (processor))

(defgeneric gprs (processor))

(defgeneric argument-registers (processor))

(defgeneric available-registers (processor))

(defgeneric callee-saved-registers (processor))

