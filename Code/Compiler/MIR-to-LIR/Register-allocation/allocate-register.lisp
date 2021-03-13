(cl:in-package #:sicl-register-allocation)

(defun unassigned-registers (arrangement candidates)
  (loop for candidate in candidates
        unless (member candidate (attributions arrangement)
                       :test #'eq :key #'register)
          collect candidate))
