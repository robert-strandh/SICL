(cl:in-package #:sicl-register-allocation)

(defgeneric allocate-registers-for-instruction (predecessor instruction))

(defgeneric compute-input-arrangement (predecessor instruction))
(defgeneric compute-output-arrangement (predecessor instruction))

(defmethod allocate-registers-for-instruction (predecessor instruction)
  (let ((new-predecessor (compute-input-arrangement predecessor instruction)))
    (compute-output-arrangement new-predecessor instruction)))

(defmethod compute-input-arrangement (predecessor instruction)
  (let ((result predecessor))
    (loop for input in (cleavir-ir:inputs instruction)
          when (typep input 'cleavir-ir:lexical-location)
            do (setf result (ensure-in-register input result instruction)))))
