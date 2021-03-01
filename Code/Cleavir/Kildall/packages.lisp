(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-kildall
  (:use #:cl)
  (:export #:make-pool)
  (:export #:instruction-pool #:dictionary-pool
           #:maybe-instruction-pool #:pool-present-p)
  (:export #:kildall #:transfer)
  (:export #:pool-meet #:pool<= #:object1
           #:find-in-pool #:map-into-pool #:do-into-pool
           #:with-pool-reader #:copy)
  (:export #:add-work)
  (:export #:iterate-mixin)
  (:export #:start-everywhere-mixin #:start-enter-mixin)
  (:export #:alist-pool-mixin #:instruction-variables)
  (:export #:bitset-pool-mixin)
  (:export #:interfunction-mixin
           #:enter-enclose #:return-enclose #:enclose-info))
