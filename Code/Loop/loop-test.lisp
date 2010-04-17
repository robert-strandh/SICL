(defpackage #:sicl-loop-test
    (:use #:common-lisp #:sicl-loop #:lisp-unit)
  (:shadow #:loop))

(in-package #:sicl-loop-test)

(define-test loop-until-t
  (assert-equal nil (sicl-loop:loop until t)))

(define-test loop-while-nil
  (assert-equal nil (sicl-loop:loop while nil)))
