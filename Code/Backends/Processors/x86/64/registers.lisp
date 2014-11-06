(cl:in-package #:sicl-x86-64)

(defmethod cleavir-processor-x86-64:available-registers ((processor x86-64))
  (loop for i in '(0 2 3 6 7 8 9 11 12 13 14 15)
	collect (cleavir-processor-x86-64:gpr processor i)))

(defmethod cleavir-processor-x86-64:argument-registers ((processor x86-64))
  (loop for i in '(7 6 3 2 8)
	collect (cleavir-processor-x86-64:gpr processor i)))

(defmethod cleavir-processor-x86-64:callee-saved-registers ((processor x86-64))
  (loop for i in '(1 12 13 14 15)
	collect (cleavir-processor-x86-64:gpr processor i)))
