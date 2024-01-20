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
    adjoin
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
    string stringp
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
        do (setf (clostrum:fdefinition client global-environment name)
                 (fdefinition name))))
