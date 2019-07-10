(cl:in-package #:cleavir-ir)

(defun both (x y)
  (and (not (null x)) (not (null y))))

(defun none (x y)
  (and (null x) (null y)))

(defun both-or-none (x y)
  (or (both x y) (none x y)))

(defun combine (xy x y)
  (if (none x y) xy (list x y)))
