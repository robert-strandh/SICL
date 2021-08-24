(cl:in-package #:sicl-arithmetic)

(defmethod binary-gcd ((x integer) (y integer))
  ;; Binary GCD algorithm (or "Stein's algorithm", for disambiguation)
  ;; TODO?: Could be improved by using find-first-set to remove powers of two.
  ;; TODO?: More sophisticated algorithms are more appropriate for
  ;; large bignums.
  (loop with x = (abs x) with y = (abs y) with twos = 0
        do (cond ((zerop x) (return (ash y twos)))
                 ((zerop y) (return (ash x twos)))
                 ((oddp x) (cond ((evenp y) (setf y (ash y -1)))
                                 ;; x and y are both odd, so (- x y) is even,
                                 ;; so we can throw in an extra shift early.
                                 ((< x y) (setf y (ash (- y x) -1)))
                                 (t (setf x (ash (- x y) -1)))))
                 ((oddp y) (setf x (ash x -1)))
                 (t (setf x (ash x -1) y (ash y -1) twos (1+ twos))))))
