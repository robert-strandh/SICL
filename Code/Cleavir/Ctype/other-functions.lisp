(in-package #:cleavir-ctype)

(defun conjoin (system &rest ctypes)
  (cond ((null ctypes) (top system))
        ((null (cl:rest ctypes)) (first ctypes))
        (t
         (reduce (lambda (ct1 ct2) (conjoin/2 ct1 ct2 system))
                 ctypes))))
(defun disjoin (system &rest ctypes)
  (cond ((null ctypes) (bottom system))
        ((null (cl:rest ctypes)) (first ctypes))
        (t
         (reduce (lambda (ct1 ct2) (disjoin/2 ct1 ct2 system))
                 ctypes))))
