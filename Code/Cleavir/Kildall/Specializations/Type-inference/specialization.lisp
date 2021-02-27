(in-package #:cleavir-kildall-type-inference)

(defclass type-inference
    (cleavir-kildall:iterate-mixin
     cleavir-liveness:live-before-mixin
     cleavir-kildall:start-enter-mixin
     cleavir-kildall:interfunction-mixin)
  ((%environment :initarg :env :reader environment)
   (%typecache :initform (make-hash-table :test #'equal)
               :reader typecache)))

(declaim (inline approximate-type approximate-values))

(defun approximate-type (specialization specifier)
  (cleavir-type-descriptors:approximate-type
   specifier (environment specialization)
   (typecache specialization)))

(declaim (inline approximate-values))
(defun approximate-values (specialization required optional rest)
  (cleavir-type-descriptors:approximate-values
   required optional rest
   (environment specialization) (typecache specialization)))

(declaim (inline top-p bottom-p values-top-p))
(defun top-p (specialization descriptor)
  (cleavir-type-descriptors:top-p
   descriptor (environment specialization)))
(defun bottom-p (specialization descriptor)
  (cleavir-type-descriptors:bottom-p
   descriptor (environment specialization)))

(defun bottom (specialization)
  (cleavir-type-descriptors:bottom (environment specialization)))

(defun values-top () (cleavir-type-descriptors:values-top))
(defun values-top-p (specialization values-descriptor)
  (cleavir-type-descriptors:values-top-p
   values-descriptor (environment specialization)))

(defun values-bottom () (cleavir-type-descriptors:values-bottom))

(defun values-binary-meet (specialization v1 v2)
  (cleavir-type-descriptors:values-binary-meet
   v1 v2 (environment specialization)))

(defun binary-meet (s t1 t2)
  (cleavir-type-descriptors:binary-meet
   t1 t2 (environment s) (typecache s)))
(defun binary-join (s t1 t2)
  (cleavir-type-descriptors:binary-join
   t1 t2 (environment s) (typecache s)))

(defun values-nth (s values-descriptor n)
  (cleavir-type-descriptors:values-nth
   values-descriptor n (environment s)))

(defun make-values (s required optional rest)
  (cleavir-type-descriptors:make-values-descriptor
   required optional rest (environment s) (typecache s)))

(defun descriptor-box (descriptor)
  (cleavir-type-descriptors:descriptor-box descriptor))
(defun descriptor-unbox (descriptor)
  (cleavir-type-descriptors:descriptor-unbox descriptor))

(defun make-function-descriptor (lambda-list return-values)
  (cleavir-type-descriptors:make-function-descriptor
   lambda-list return-values))

(defun return-values (descriptor)
  (cleavir-type-descriptors:return-values descriptor))

(defmethod cleavir-kildall:object-meet ((s type-inference) t1 t2)
  ;; "meet" is a join due to having to use the dual lattice because
  ;; of how kildall works.
  (cleavir-type-descriptors:binary-join
   t1 t2 (environment s) (typecache s)))

(defmethod cleavir-kildall:object<= ((s type-inference) t1 t2)
  ;; similarly backwards.
  (cleavir-type-descriptors:sub-descriptor-p
   t2 t1 (environment s) (typecache s)))
