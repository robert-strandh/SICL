(cl:in-package #:sicl-new-boot-phase-2)

;;; Before we can start creating generic functions, we need to define
;;; method combinations, but method combinations reqiure a few
;;; functions from Ecclesia.  But we can't load Ecclesia first because
;;; it contains definitions of generic functions.  As it turn out, we
;;; loaded Ecclesia in phase 1, so it is present in environment E1.
;;; So what we do here is to incorporate the functions we need in E2
;;; taken from E1.

(defun define-ecclesia-functions (client e1 e2)
  (let ((symbol
          (sb:intern-parcl-symbol
           client "ECCLESIA" "PROPER-LIST-P")))
    (setf (clo:fdefinition client e2 symbol)
          (clo:fdefinition client e1 symbol)))
  (let ((symbol
          (sb:intern-parcl-symbol
           client "ECCLESIA" "EXTRACT-LAMBDA-LIST-VARIABLES")))
    (setf (clo:fdefinition client e2 symbol)
          (clo:fdefinition client e1 symbol)))
  (let ((symbol
          (sb:intern-parcl-symbol
           client "ECCLESIA" "SEPARATE-FUNCTION-BODY")))
    (setf (clo:fdefinition client e2 symbol)
          (clo:fdefinition client e1 symbol))))
