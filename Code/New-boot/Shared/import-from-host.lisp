(cl:in-package #:sicl-new-boot)

(defparameter *host-function-names*
  '(;; Arithmetic
    + - * / floor ceiling 1+ 1- = /= < > <= >= max min evenp oddp
    zerop plusp minusp
    numberp integerp random
    ;; Conses
    cons list list* null endp
    car cdr caar cadr cdar cddr
    caaar caadr cadar caddr cdaar cdadr cddar cdddr
    cddadr
    first second third fourth fifth sixth seventh eighth ninth tenth
    rest last butlast nth nthcdr append getf
    consp listp atom member mapcar mapc set-difference set-exclusive-or
    assoc ldiff tailp list-length make-list union
    adjoin copy-list
    ;; Sequences
    elt subseq reduce length reverse nreverse
    count count-if count-if-not
    find find-if find-if-not
    position position-if position-if-not
    remove remove-if remove-if-not
    delete delete-if delete-if-not
    remove-duplicates sort
    ;; Symbols
    gensym symbolp symbol-name
    ;; Strings
    string stringp string-downcase
    ;; Characters
    char
    ;; Conditions
    error warn break assert
    ;; Evaluation and compilation
    constantp
    ;; Data and control flow
    values not funcall apply eq eql equal identity functionp every
    ;; Printer
    format
    ;; Environment
    documentation
    ;; Hash tables
    make-hash-table gethash hash-table-count))

(defun import-host-functions (client global-environment)
  (loop for name in *host-function-names*
        do (setf (clo:fdefinition client global-environment name)
                 (fdefinition name))))

(defparameter *host-setf-functions*
  `(((setf car)
     ,(lambda (object cons)
        (setf (car cons) object)))
    ((setf cdr)
     ,(lambda (object cons)
        (setf (cdr cons) object)))
    ((setf cadr)
     ,(lambda (object cons)
        (setf (cadr cons) object)))
    ((setf cddr)
     ,(lambda (object cons)
        (setf (cddr cons) object)))
    ((setf first)
     ,(lambda (object cons)
        (setf (first cons) object)))))

(defun define-setf-functions (client global-environment)
  (loop for (name definition) in *host-setf-functions*
        do (setf (clo:fdefinition client global-environment name)
                 definition)))
