(cl:in-package #:common-lisp-user)

(defpackage #:target-common-lisp
  (:use)
  (:import-from #:common-lisp
                .
                #.(let ((result '()))
                    (do-external-symbols (symbol (find-package '#:common-lisp))
                      (when (or (member symbol lambda-list-keywords)
                                (special-operator-p symbol))
                        (push (symbol-name symbol) result)))
                    result))
  (:export
   .
   #.(let ((result '()))
       (do-external-symbols (symbol (find-package '#:common-lisp))
         (push (symbol-name symbol) result))
       result)))
