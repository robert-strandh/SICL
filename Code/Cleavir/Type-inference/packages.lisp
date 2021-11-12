(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-type-inference
  (:use #:common-lisp)
  (:export #:approximate-type #:canonicalize-type
           #:top-p #:bottom-p
           #:binary-join #:binary-meet #:difference
           #:join #:meet
           #:descriptor-box #:descriptor-unbox
           #:approximate-values #:values-nth #:values-rest-p
           #:values-top-p #:values-bottom-p
           #:values-required #:values-required-count
           #:values-binary-meet #:values-binary-join
           #:values-meet #:values-join
           #:values-top #:values-bottom
           #:values-descriptor->type
           #:infer-types #:arc-bag
           #:instruction-input #:find-type
           #:type-missing
           #:type-missing-location #:type-missing-bag))
