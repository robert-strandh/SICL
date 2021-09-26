(cl:in-package #:sicl-code-generation)

(defun translate-datum (datum)
  (if (typep datum 'cleavir-ir:immediate-input)
      (make-instance 'cluster:immediate-operand
        :value (cleavir-ir:value datum))
      (make-instance 'cluster:gpr-operand
        :code-number (x86-64::register-number datum)
        :size 64)))
