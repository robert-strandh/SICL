(cl:in-package #:common-lisp-user)

(defpackage #:sicl-environment
  (:use #:common-lisp)
  (:shadow #:get-setf-expansion type)
  (:shadowing-import-from
   #:clostrum
   .
   #.(loop for symbol being each external-symbol in '#:clostrum
           unless (member symbol '(clostrum:run-time-environment
                                   clostrum:compilation-environment))
             collect (symbol-name symbol)))
  (:export #:global-environment
           #:client
           #:base-run-time-environment
           #:run-time-environment
           #:evaluation-environment
           #:compilation-environment
           #:function-descriptino
           #:simple-function-description
           #:generic-function-description
           #:lambda-list
           #:class-name
           #:method-class-name
           #:method-combination-info
           #:get-setf-expansion
           #:type
           .
           #.(loop for symbol being each external-symbol in '#:clostrum
                   unless (member symbol '(clostrum:run-time-environment
                                           clostrum:compilation-environment))
                     collect (symbol-name symbol))))
