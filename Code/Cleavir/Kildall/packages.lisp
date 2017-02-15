(cl:in-package #:common-lisp-user)

;;;; Implementation of Kildall's "Algorithm A":
;;;; G.A. Kildall, "A Unified Approach to Global Program Optimization." Proceedings of the First ACM Symposium on Principles of Programming Languages,194-206, 1973.
;;;; It's a very general algorithm for optimization information on
;;;; program structures like HIR.
;;;; See liveness.lisp for an example.

(defpackage #:cleavir-kildall
  (:use #:cl)
  (:export #:instruction-pool)
  (:export #:pool-meet #:pool<=)
  (:export #:specialization #:forward-traverse #:reverse-traverse
	   #:forward-spread-traverse #:reverse-spread-traverse
	   #:forward-single-traverse
	   #:entry-pool)
  (:export #:transfer #:process-transfer
	   #:kildall)
  (:export #:interfunction-mixin #:interfunction-once-mixin
           #:transfer-enclose #:dictionary->info))

(defpackage #:cleavir-set
  (:use #:cl)
  (:shadow #:union #:intersection #:subsetp #:adjoin #:remove)
  (:export #:union #:intersection #:difference #:exclusive-or
	   #:subsetp #:adjoin #:remove #:in-set-p
	   #:make-set #:set->list)
  (:export #:listset #:bitset)
  (:export #:instruction-universe #:data-universe
	   #:universe-of-things))
