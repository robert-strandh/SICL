(cl:in-package #:sicl-new-boot)

(defparameter *host-function-names*
  '(;; Arithmetic
    + - * / floor ceiling 1+ 1- = /= < > <= >= max min evenp oddp
    ;; Conses
    cons list list*
    car cdr caar cadr cdar cddr
    caaar caadr cadar caddr cdaar cdadr cddar cdddr
    first second third fourth fifth sixth seventh eighth ninth tenth
    nth nthcdr
    ;; Sequences
    elt subseq reduce length reverse nreverse
    count count-if count-if-not
    find find-if find-if-not
    position position-if position-if-not
    remove remove-if remove-if-not
    delete delete-if delete-if-not))
  

(defun import-from-host (client global-environment)
  (loop for name in *host-function-names*
        do (setf (clostrum:fdefinition client global-environment name)
                 (fdefinition name))))
