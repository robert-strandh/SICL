(cl:in-package #:sicl-array)

(defun array-dimension (array axis-number)
  (let ((dimensions (array-dimensions array)))
    (if (or (minusp axis-number)
            (>= axis-number (length dimensions)))
        ;; FIXME: signal a specific condition.
        (error "Invalid axis number")
        (nth axis-number dimensions))))
