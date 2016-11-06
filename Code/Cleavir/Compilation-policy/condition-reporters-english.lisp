(cl:in-package #:cleavir-compilation-policy)

(defmethod acclimation:report-condition
    ((condition bad-optimize-value) stream (language acclimation:english))
  (format stream "Ignoring bad optimize value for ~s: ~s~@
                  Expected ~s"
	  (first (specifier condition))
	  (second (specifier condition))
	  (expected-type condition)))

(defmethod acclimation:report-condition
    ((condition unknown-optimize-quality) stream (language acclimation:english))
  (format stream "Ignoring unknown OPTIMIZE quality ~s"
	  (first (specifier condition))))

(defmethod acclimation:report-condition
    ((condition no-policy-computer) stream (language acclimation:english))
  (format stream "~s is a defined policy quality for ~s, but there~@
                  is no method on COMPUTE-POLICY-QUALITY for it."
	  (quality condition) (environment condition)))
