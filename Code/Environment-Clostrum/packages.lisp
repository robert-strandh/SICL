(cl:in-package #:common-lisp-user)

(defpackage #:sicl-environment
  (:use #:common-lisp)
  (:shadowing-import-from
   #:clostrum
   .
   #.(loop for symbol being each external-symbol in '#:clostrum
                        collect symbol))
  (:export #:global-environment
           .
           #.(loop for symbol being each external-symbol in '#:clostrum
                   collect symbol)))
