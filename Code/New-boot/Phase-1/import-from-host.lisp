(cl:in-package #:sicl-new-boot-phase-1)

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
    assoc ldiff tailp list-length
    ;; Sequences
    elt subseq reduce length reverse nreverse
    count count-if count-if-not
    find find-if find-if-not
    position position-if position-if-not
    remove remove-if remove-if-not
    delete delete-if delete-if-not
    remove-duplicates
    ;; Symbols
    gensym symbolp symbol-name
    ;; Strings
    string stringp
    ;; Characters
    char
    ;; Conditions
    error warn break
    ;; Evaluation and compilation
    constantp
    ;; Data and control flow
    values not funcall apply eq eql equal identity functionp every
    ;; Printer
    format
    ;; Environment
    documentation
    ;; Hash tables
    make-hash-table gethash hash-table-count
    ;; Objects
    initialize-instance reinitialize-instance shared-initialize
    add-method))

(defun import-host-functions (client global-environment)
  (loop for name in *host-function-names*
        do (setf (clostrum:fdefinition client global-environment name)
                 (fdefinition name))))

(defun import-host-classes (client global-environment)
  (loop for name in '(symbol)
        do (setf (clostrum:find-class client global-environment name)
                 (find-class 't))))

(defparameter *host-setf-functions*
  `(((setf car)
     ,(lambda (object cons)
        (setf (car cons) object)))
    ((setf cdr)
     ,(lambda (object cons)
        (setf (cdr cons) object)))
    ((setf first)
     ,(lambda (object cons)
        (setf (first cons) object)))
    ((setf gethash)
     ,(lambda (object key table)
        (setf (gethash key table) object)))
    ((setf documentation)
     ,(lambda (documentation object documentation-type)
        (setf (documentation object documentation-type) documentation)))))

(defun define-setf-functions (client global-environment)
  (loop for (name definition) in *host-setf-functions*
        do (setf (clostrum:fdefinition client global-environment name)
                 definition)))

(defun import-from-host (client global-environment)
  (import-host-functions client global-environment)
  (import-host-classes client global-environment)
  (define-setf-functions client global-environment))
