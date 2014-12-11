(in-package #:sicl-loop-test)

(defun loop-until-t ()
  (assert (equal (loop until t)
		 nil)))

(defun loop-while-nil ()
  (assert (equal (loop while nil)
		 nil)))

(defun loop-until-expr ()
  (assert (equal (let ((n 0))
		   (loop until (= (incf n) 10))
		   n)
		 10)))

(defun loop-while-expr ()
  (assert (equal (let ((n 0))
		   (loop while (< (incf n) 10))
		   n)
		 10)))

(defun loop-do ()
  (assert (equal (let ((n 0))
		   (block abc
		     (loop do (progn (incf n)
				     (when (= n 10)
				       (return-from abc nil)))))
		   n)
		 10)))

;; (defun loop-until-do ()
;;   (assert (equal (let ((n 0))
;; 		   (loop until (= n 10)
;; 			 do (incf n))
;; 		   n)
;; 		 10)))

;; (defun loop-repeat-do ()
;;   (assert (equal (let ((n 0))
;; 		   (loop repeat 5
;; 			 do (incf n 2))
;; 		   n)
;; 		 10)))

;; (defun loop-with-repeat-do ()
;;   (assert (equal (let ((n 0))
;; 		   (loop with step = 2
;; 			 repeat 5
;; 			 do (incf n step))
;; 		   n)
;; 		 10)))

;; (defun loop-initially-repeat-do ()
;;   (assert (equal (let ((n 0))
;; 		   (loop initially (incf n 10)
;; 			 repeat 2
;; 			 do (setf n (* n 2)))
;; 		   n)
;; 		 40)))

;; (defun loop-repeat-do-finally ()
;;   (assert (equal (let ((n 0))
;; 		   (loop repeat 2
;; 			 do (incf n 10)
;; 			 finally (setf n (* n 2)))
;; 		   n)
;; 		 40)))

;; (defun loop-with-repeat-do-collect-finally ()
;;   (assert (equal (let ((result nil))
;; 		   (loop with n = 0
;; 			 repeat 4
;; 			 do (incf n)
;; 			 collect n into foo
;; 			 finally (setf result foo))
;; 		   result)
;; 		 '(1 2 3 4))))

;; (defun loop-repeat-sum-finally ()
;;   (assert (equal (let ((result nil))
;; 		   (loop repeat 5
;; 			 sum 2 into foo
;; 			 finally (setf result foo))
;; 		   result)
;; 		 10)))

(defun loop-test ()
  (loop-until-t)
  (loop-while-nil)
  (loop-until-expr)
  (loop-while-expr)
  (loop-do))
