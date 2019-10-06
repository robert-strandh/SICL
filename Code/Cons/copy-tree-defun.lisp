(cl:in-package #:sicl-cons)

;;; At the moment, we use recursion for both the CAR and the CDR, but
;;; it would probably be more efficient to use recursion for the CAR
;;; and iteration for the CDR, assuming trees are typically wider than
;;; they are deep.

(defun copy-tree (tree)
  (if (atom tree)
      tree
      (cons (copy-tree (car tree)) (copy-tree (cdr tree)))))
