(defpackage #:sicl-loop-test
    (:use #:common-lisp #:sicl-loop #:lisp-unit)
  (:shadow #:loop))

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
