(cl:in-package #:sicl-x86-64)

(defmethod cleavir-processor-x86-64:available-registers ((processor x86-64))
  (loop for i in '(0 2 3 6 7 8 9 11 12 13 14 15)
	collect (cleavir-processor-x86-64:gpr processor i)))

