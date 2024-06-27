(cl:in-package #:sicl-new-boot)

(defparameter *host-function-names*
  '(;; Arithmetic
    + - * / floor ceiling 1+ 1- = /= < > <= >= max min evenp oddp
    zerop plusp minusp expt
    numberp integerp realp rationalp floatp random
    logcount lognot integer-length
    ;; Conses
    cons list list* null endp
    car cdr caar cadr cdar cddr
    caaar caadr cadar caddr cdaar cdadr cddar cdddr
    cddadr
    first second third fourth fifth sixth seventh eighth ninth tenth
    rest last butlast nth nthcdr append getf
    consp listp atom member member-if mapcar mapc set-difference
    set-exclusive-or
    assoc ldiff tailp list-length make-list union intersection
    adjoin copy-list
    subsetp
    ;; Sequences
    elt subseq reduce length reverse nreverse
    count count-if count-if-not
    find find-if find-if-not
    position position-if position-if-not
    remove remove-if remove-if-not
    delete delete-if delete-if-not
    remove-duplicates sort map substitute
    ;; Symbols
    gensym symbolp symbol-name get
    ;; Strings
    string stringp string-downcase string=
    ;; Characters
    char characterp
    ;; Conditions
    error warn break assert
    ;; Evaluation and compilation
    constantp
    ;; Data and control flow
    values values-list not apply eq eql equal identity
    functionp every notany some constantly
    ;; Printer
    format finish-output
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
