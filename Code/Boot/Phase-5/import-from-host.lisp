(cl:in-package #:sicl-boot-phase-5)

(defun import-number-functions (e5)
  (import-functions-from-host
   '(+ - * < <= = > >= /= floor 1+ 1-
     ;; ZEROP is used in CLOS to compute the discriminating function.
     zerop
     ;; ODDP is used at compile time in some macro expanders
     oddp
     ;; EVENP is used in the expander for the short form of
     ;; DEFINE-METHOD-COMBINATION.
     evenp
     ;; EXPT is used to define the array-size limit.
     expt
     ;; RANDOM is used to create a hash code for standard
     ;; objects.
     random
     ;; MINUSP is used by FLOOR and other arithmetic functions.
     minusp
     ;; INTEGERP is used by ARRAY-ROW-MAJOR-INDEX, and by FORMAT at
     ;; run time.
     integerp
     ;; NUMBERP is used by the LOOP run time.
     numberp)
   e5))

(defun import-misc (e5)
  (import-functions-from-host
   '(coerce)
   e5)
  (setf (env:macro-function (env:client e5) e5 'defpackage)
        (lambda (form env)
          (declare (ignore env))
          (eval form)
          nil))
  ;; Fake PROGV for now.
  (setf (env:macro-function (env:client e5) e5 'progv)
        (lambda (form env)
          (declare (ignore env))
          (cons 'progn (rest form)))))

(defun import-data-and-control-flow (e5)
  (import-functions-from-host
   '(not eq eql equal values constantly)
   e5))

(defun import-from-host (e5)
  (import-misc e5)
  (import-number-functions e5)
  (import-data-and-control-flow e5))
