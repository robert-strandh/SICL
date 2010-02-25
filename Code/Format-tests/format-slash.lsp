;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Aug 21 09:51:08 2004
;;;; Contains: Tests for format directive ~/.../

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

(def-pprint-test format./.1
  (format nil "~/pprint-linear/" 1)
  "1")

(def-pprint-test format./.2
  (format nil "~/pprint-linear/" 2)
  "2"
  :pretty nil)

(def-pprint-test format./.3
  (format nil "~/pprint-linear/" '(17))
  "17")

(def-pprint-test format./.4
  (format nil "~:/pprint-linear/" '(17))
  "(17)")

(def-pprint-test format./.5
  (format nil "~@/pprint-linear/" 1)
  "1")

(def-pprint-test format./.6
  (format nil "~@:/pprint-linear/" 1)
  "1")

(def-pprint-test format./.7
  (format nil "~/PPRINT-LINEAR/" 1)
  "1")

(def-pprint-test format./.8
  (format nil "~/pPrINt-lINeaR/" 1)
  "1")

(def-pprint-test format./.9
  (progn
    (setf (symbol-function 'FUNCTION-FOR-FORMAT-SLASH-9) #'pprint-linear)
    (format nil "~/CL-TEST::FUNCTION-FOR-FORMAT-SLASH-9/" 1))
  "1")

;;; Single : doesn't mean it has to be exported
(def-pprint-test format./.10
  (progn
    (setf (symbol-function 'FUNCTION-FOR-FORMAT-SLASH-10) #'pprint-linear)
    (format nil "~/cl-test:FUNCTION-FOR-FORMAT-SLASH-10/" 1))
  "1")

(def-pprint-test format./.11
  (progn
    (setf (symbol-function '|FUNCTION:FOR::FORMAT:SLASH:11|) #'pprint-linear)
    (format nil "~/cL-tESt:FUNCTION:FOR::FORMAT:SLASH:11/" 1))
  "1")

(def-pprint-test format./.12
  (format nil "~<~/pprint-tabular/~:>" '((|M|)))
  "M")

(def-pprint-test format./.13
  (format nil "~<~:/pprint-tabular/~:>" '((|M|)))
  "(M)")

(def-pprint-test format./.14
  (format nil "~<~:@/pprint-tabular/~:>" '((|M|)))
  "(M)")

(def-pprint-test format./.15
  (format nil "~<~@/pprint-tabular/~:>" '((|M|)))
  "M")

(def-pprint-test format./.16
  (format nil "~<~4:/pprint-tabular/~:>" '((|M| |M|)))
  "(M   M)")

(def-pprint-test format./.17
  (format nil "~<~v:/pprint-tabular/~:>" '(nil (|M| |M|)))
  "(M               M)")

(def-pprint-test format./.18
  (format nil "~<~v:/pprint-tabular/~:>" '(3 (|M| |M|)))
  "(M  M)")

(declaim (special *expected-args*))

(def-pprint-test format./.19
  (progn
   (setf (symbol-function 'function-for-format-slash-19)
	 #'(lambda (stream &rest args)
	     (assert (= (length args) (length *expected-args*)))
	     (assert (equal (car args) (car *expected-args*)))
	     (assert (if (cadr args) (cadr *expected-args*)
		       (not (cadr *expected-args*))))
	     (assert (if (caddr args) (caddr *expected-args*)
		       (not (caddr *expected-args*))))
	     (apply #'pprint-fill stream (subseq args 0 3))))
   (list
    (let ((*expected-args* '(1 nil nil)))
      (format nil "~/cl-test::function-for-format-slash-19/" 1))
    (let ((*expected-args* '(2 t nil)))
      (format nil "~:/cl-test::function-for-format-slash-19/" 2))
    (let ((*expected-args* '(3 nil t)))
      (format nil "~@/cl-test::function-for-format-slash-19/" 3))
    (let ((*expected-args* '(4 t t)))
      (format nil "~:@/cl-test::function-for-format-slash-19/" 4))
    (let ((*expected-args* '(5 t t)))
      (format nil "~@:/cl-test::function-for-format-slash-19/" 5))
    (let ((*expected-args* '(6 t t 18)))
      (format nil "~18@:/cl-test::function-for-format-slash-19/" 6))
    (let ((*expected-args* '(7 nil nil 19)))
      (format nil "~v/cl-test::function-for-format-slash-19/" 19 7))
    (let ((*expected-args* '(8 t nil #\X)))
      (format nil "~'X:/cl-test::function-for-format-slash-19/" 8))
    (let ((*expected-args* '(9 nil t #\,)))
      (format nil "~',@/cl-test::function-for-format-slash-19/" 9))
    (let ((*expected-args* '(10 nil t -1)))
      (format nil "~-1@/cl-test::function-for-format-slash-19/" 10))
    (let ((*expected-args* '(11 nil t 1 2 3 4 5 6 7 8 9 10)))
      (format nil "~1,2,3,4,5,6,7,8,9,10@/cl-test::function-for-format-slash-19/" 11))
    (let ((*expected-args* '(12 nil t 1 2 3 4 5 6 7 8 9 10)))
      (format nil "~v,v,v,v,v,v,v,v,v,v@/cl-test::function-for-format-slash-19/" 1 2 3 4 5 6 7 8 9 10 12))
    ))
  ("1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12"))





