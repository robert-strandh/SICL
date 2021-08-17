(cl:in-package #:sicl-arithmetic)

(defmethod binary-lcm ((x integer) (y integer))
  (if (or (zerop x) (zerop y))
      0
      ;; Rather than the obvious (/ (abs (* a b)) (gcd a b)), we divide the
      ;; GCD out first, so that we don't need to compute a large intermediate
      ;; which we then immediately shrink.
      ;; TODO?: Could use an exact division algorithm for the truncation.
      (let ((x (abs x)) (y (abs y)))
        (multiple-value-bind (high low) (if (> x y) (values x y) (values y x))
          (* (truncate high (gcd x y)) low)))))
