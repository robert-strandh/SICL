(cl:in-package #:sicl-new-boot-phase-1)

(defparameter *host-function-names*
  '(;; Arithmetic
    + - * / floor ceiling 1+ 1- = /= < > <= >= max min evenp oddp
    random
    ;; Conses
    cons list list* null endp
    car cdr caar cadr cdar cddr
    caaar caadr cadar caddr cdaar cdadr cddar cdddr
    first second third fourth fifth sixth seventh eighth ninth tenth
    rest nth nthcdr
    consp atom
    ;; Sequences
    elt subseq reduce length reverse nreverse
    count count-if count-if-not
    find find-if find-if-not
    position position-if position-if-not
    remove remove-if remove-if-not
    delete delete-if delete-if-not
    ;; Symbols
    gensym
    ;; Strings
    string
    ;; Conditions
    error
    ;; Data and control flow
    funcall))

(defun import-host-functions (client global-environment)
  (loop for name in *host-function-names*
        do (setf (clostrum:fdefinition client global-environment name)
                 (fdefinition name))))

(defparameter *host-setf-functions*
  `(((setf car)
     ,(lambda (object cons)
        (setf (car cons) object)))
    ((setf cdr)
     ,(lambda (object cons)
        (setf (cdr cons) object)))
    ((setf first)
     ,(lambda (object cons)
        (setf (first cons) object)))))

(defun define-setf-functions (client global-environment)
  (loop for (name definition) in *host-setf-functions*
        do (setf (clostrum:fdefinition client global-environment name)
                 definition)))

(defun import-from-host (client global-environment)
  (import-host-functions client global-environment)
  (define-setf-functions client global-environment))
