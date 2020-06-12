(in-package #:cleavir-ctype)

;;; NOTE: Maybe should accept no ctypes (and return top)?
(defun conjoin (system ctype &rest more-ctypes)
  (reduce (lambda (ct1 ct2) (conjoin/2 ct1 ct2 system))
          more-ctypes
          :initial-value ctype))
(defun disjoin (system ctype &rest more-ctypes)
  (reduce (lambda (ct1 ct2) (disjoin/2 ct1 ct2 system))
          more-ctypes
          :initial-value ctype))
