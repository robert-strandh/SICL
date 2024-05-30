(cl:in-package #:sicl-new-boot)

(eval-when (:compile-toplevel) (enable-parcl-symbols client))

;;; Before we can start creating generic functions, we need to define
;;; method combinations, but method combinations reqiure a few
;;; functions from Ecclesia.  But we can't load Ecclesia first because
;;; it contains definitions of generic functions.  As it turn out, we
;;; loaded Ecclesia in phase 1, so it is present in environment E1.
;;; So what we do here is to incorporate the functions we need in E
;;; taken from E1.

(defun define-ecclesia-functions (client e1 e)
  (let ((symbol @ecclesia:proper-list-p))
    (setf (clo:fdefinition client e symbol)
          (clo:fdefinition client e1 symbol)))
  (let ((symbol @ecclesia:extract-lambda-list-variables))
    (setf (clo:fdefinition client e symbol)
          (clo:fdefinition client e1 symbol)))
  (let ((symbol @ecclesia:separate-function-body))
    (setf (clo:fdefinition client e symbol)
          (clo:fdefinition client e1 symbol)))
  (let ((symbol @ecclesia:list-structure))
    (setf (clo:fdefinition client e symbol)
          (clo:fdefinition client e1 symbol))))
