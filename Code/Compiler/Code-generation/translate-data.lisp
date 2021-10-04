(cl:in-package #:sicl-code-generation)

(defgeneric translate-datum (datum))

(defmethod translate-datum ((datum cleavir-ir:immediate-input))
  (make-instance 'cluster:immediate-operand
    :value (cleavir-ir:value datum)))

(defmethod translate-datum ((datum cleavir-ir:register-location))
  (make-instance 'cluster:gpr-operand
    :code-number (x86-64::register-number datum)
    :size 64))

(defmethod translate-datum ((datum sicl-ir:effective-address))
  (make-instance 'cluster:memory-operand
    :base-register (x86-64::register-number (sicl-ir::base datum))
    :index-register
    (let ((r (sicl-ir::offset datum)))
      (if (null r) nil (x86-64::register-number r)))
    :scale (sicl-ir::scale datum)
    :displacement (sicl-ir::displacement datum)
    :size 64))
