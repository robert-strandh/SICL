(cl:in-package #:sicl-register-allocation)

;;; This function currently returns how many stack slots were used,
;;; and whether or not we need to spill argument registers to the
;;; stack.

(defun do-register-allocation (enter-instruction)
  (let ((*temporary-argument-locations*
          (make-temporary-argument-locations))
        (*temporary-argument-count-location*
          (make-temporary-argument-count-location))
        (*non-constant-argument-instruction-p* nil))
    (preprocess-instructions enter-instruction)
    (let ((back-arcs (find-back-arcs enter-instruction))
          (*input-pools* (make-hash-table :test #'eq))
          (*output-pools* (make-hash-table :test #'eq))
          (*input-arrangements* (make-hash-table :test #'eq))
          (*output-arrangements* (make-hash-table :test #'eq)))
      (compute-estimated-distance-to-use enter-instruction back-arcs)
      (let ((stack-frame-size (allocate-registers-for-instructions enter-instruction)))
        (introduce-registers enter-instruction)
        (values stack-frame-size
                *non-constant-argument-instruction-p*)))))
