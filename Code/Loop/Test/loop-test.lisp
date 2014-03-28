(in-package #:sicl-loop-test)

(define-test loop-until-t
  (assert-equal nil (sicl-loop:loop until t)))

(define-test loop-while-nil
  (assert-equal nil (sicl-loop:loop while nil)))

(define-test loop-until-expr
  (assert-equal 10
                (let ((n 0))
                  (sicl-loop:loop until (= (incf n) 10))
                  n)))

(define-test loop-while-expr
  (assert-equal 10
                (let ((n 0))
                  (sicl-loop:loop while (< (incf n) 10))
                  n)))

(define-test loop-until-do
  (assert-equal 10
                (let ((n 0))
                  (sicl-loop:loop until (= n 10)
                                  do (incf n))
                  n)))

(define-test loop-repeat-do
  (assert-equal 10
                (let ((n 0))
                  (sicl-loop:loop repeat 5
                                  do (incf n 2))
                  n)))

(define-test loop-with-repeat-do
  (assert-equal 10
                (let ((n 0))
                  (sicl-loop:loop with step = 2
                                  repeat 5
                                  do (incf n step))
                  n)))

(define-test loop-initially-repeat-do
  (assert-equal 40
                (let ((n 0))
                  (sicl-loop:loop initially (incf n 10)
                                  repeat 2
                                  do (setf n (* n 2)))
                  n)))

(define-test loop-repeat-do-finally
  (assert-equal 40
                (let ((n 0))
                  (sicl-loop:loop repeat 2
                                  do (incf n 10)
                                  finally (setf n (* n 2)))
                  n)))

(define-test loop-with-repeat-do-collect-finally
  (assert-equal '(1 2 3 4)
                (let ((result nil))
                  (sicl-loop:loop with n = 0
                                  repeat 4
                                  do (incf n) 
                                  collect n into foo
                                  finally (setf result foo))
                  result)))

(define-test loop-repeat-sum-finally
  (assert-equal 10
                (let ((result nil))
                  (sicl-loop:loop repeat 5
                                  sum 2 into foo
                                  finally (setf result foo))
                  result)))
