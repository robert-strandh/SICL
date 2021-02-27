(cl:in-package #:common-lisp-user)

;;;; Implementation of Kildall's "Algorithm A":
;;;;
;;;; G.A. Kildall, "A Unified Approach to Global Program
;;;; Optimization." Proceedings of the First ACM Symposium on
;;;; Principles of Programming Languages,194-206, 1973.
;;;;
;;;; It's a very general algorithm for optimization information on
;;;; program structures like HIR.
;;;; See liveness.lisp for an example.

(defpackage #:cleavir-kildall
  (:use #:cl)
  (:export #:make-pool)
  (:export #:instruction-pool #:dictionary-pool
           #:maybe-instruction-pool #:pool-present-p)
  (:export #:kildall #:transfer)
  (:export #:object-meet #:object<= #:object1
           #:find-in-pool #:map-into-pool #:do-into-pool
           #:with-pool-reader #:copy)
  (:export #:add-work)
  (:export #:iterate-mixin)
  (:export #:start-everywhere-mixin #:start-enter-mixin)
  (:export #:alist-pool-mixin #:instruction-variables)
  (:export #:bitset-pool-mixin)
  (:export #:interfunction-mixin
           #:enter-enclose #:return-enclose #:enclose-info))
