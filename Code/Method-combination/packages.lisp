(cl:in-package #:common-lisp-user)

(defpackage #:sicl-method-combination
  (:use #:common-lisp)
  (:export #:method-combination-template
           #:find-method-combination
           #:effective-method-form-function
           #:define-method-combination-expander
           #:find-method-combination-template))
