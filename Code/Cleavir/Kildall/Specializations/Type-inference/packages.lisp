(defpackage #:cleavir-kildall-type-inference
  (:use #:cl)
  (:export #:infer-types #:insert-type-checks))

(defpackage #:cleavir-type-descriptors
  (:use #:cl)
  (:export #:approximate-type #:approximate-values)
  (:export #:top-p #:bottom-p #:bottom)
  (:export #:binary-join #:binary-meet #:sub-descriptor-p)
  (:export #:make-values-descriptor #:values-nth
           #:values-binary-meet
           #:values-bottom #:values-top #:values-top-p)
  (:export #:descriptor-box #:descriptor-unbox)
  (:export #:make-function-descriptor #:return-values)
  (:export #:descriptor->specifier))
