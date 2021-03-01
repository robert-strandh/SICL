(in-package #:cleavir-value-numbering)

(defclass value-numbering
    (cleavir-kildall:iterate-mixin
     cleavir-kildall:start-enter-mixin
     cleavir-liveness:live-before-mixin)
  ())

(defmethod cleavir-kildall:find-in-pool
    ((s value-numbering) (location cleavir-ir:constant-input) pool)
  (declare (ignore pool))
  ;; use the location as a value "number". This prevents it from taking up more numbers during loops.
  (list location))
(defmethod cleavir-kildall:find-in-pool
    ((s value-numbering) (location cleavir-ir:immediate-input) pool)
  (declare (ignore pool))
  (list location))
(defmethod cleavir-kildall:find-in-pool
    ((s value-numbering) (location cleavir-ir:load-time-value-input) pool)
  (declare (ignore pool))
  (list location))

(defmethod cleavir-kildall:object<= ((s value-numbering) o1 o2)
  (subsetp o2 o1 :test #'equal))

(defmethod cleavir-kildall:pool-meet ((s value-numbering) o1 o2)
  (union o1 o2 :test #'equal))

(defmethod cleavir-kildall:object1 ((s value-numbering) variable)
  (declare (ignore variable))
  nil)
