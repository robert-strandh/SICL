(cl:in-package #:common-lisp-user)

(defpackage #:sicl-lexical-environment
  (:use #:common-lisp)
  (:shadowing-import-from
   #:trucler
   .
   #.(loop for symbol being each external-symbol in '#:trucler
           unless (member symbol '(trucler:generic-function-description))
             collect (symbol-name symbol)))
  (:export
   #:generic-function-description
   .
   #.(loop for symbol being each external-symbol in '#:trucler
           unless (member symbol '(trucler:generic-function-description))
             collect (symbol-name symbol))))
