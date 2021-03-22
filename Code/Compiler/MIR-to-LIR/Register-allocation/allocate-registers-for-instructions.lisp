(cl:in-package #:sicl-register-allocation)

(defgeneric allocate-registers-for-instruction (predecessor instruction))

(defgeneric compute-input-arrangement (predecessor instruction))
(defgeneric compute-output-arrangement (predecessor instruction))

(defmethod allocate-registers-for-instruction (predecessor instruction)
  (let ((new-predecessor (compute-input-arrangement predecessor instruction)))
    (compute-output-arrangement new-predecessor instruction)))
