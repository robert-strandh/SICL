(cl:in-package #:common-lisp-user)

(defpackage #:target-common-lisp
  (:use)
  (:export
   .
   #.(let ((result '()))
       (do-external-symbols (symbol (find-package '#:common-lisp))
         (push (symbol-name symbol) result))
       result)))
