(cl:in-package #:sicl-code-generation)

(defun translate-input (datum instruction)
  (if (typep datum 'cleavir-ir:immediate-input)
      (make-instance 'cluster:immediate-operand
        :value (cleavir-ir:value datum))
      (multiple-value-bind (stack-location register-number)
          (arr::find-attribution (ra::input-arrangement instruction) datum)
        (assert (not (and (null stack-location) (null register-number))))
        (if (null stack-location)
            (make-instance 'cluster:gpr-operand
              :code-number register-number
              :size 64)
            (make-instance 'cluster:memory-operand
              :base-register (x86-64::register-number x86-64::*rbp*)
              :displacement (- (* 8 stack-location)))))))

(defun translate-output (datum instruction)
  (multiple-value-bind (stack-location register-number)
      (arr::find-attribution (ra::output-arrangement instruction) datum)
    (assert (not (and (null stack-location) (null register-number))))
    (if (null stack-location)
        (make-instance 'cluster:gpr-operand
          :code-number register-number
          :size 64)
        (make-instance 'cluster:memory-operand
          :base-register (x86-64::register-number x86-64::*rbp*)
          :displacement (- (* 8 stack-location))))))
