(cl:in-package #:sicl-register-allocation)

(defgeneric allocate-registers-for-instruction (predecessor instruction))

(defgeneric compute-input-arrangement (predecessor instruction))
(defgeneric compute-output-arrangement (instruction))

(defmethod allocate-registers-for-instruction (predecessor instruction)
  (compute-input-arrangement predecessor instruction)
  (compute-output-arrangement instruction))
