(cl:in-package #:cleavir-compilation-policy)

;;; Compute the value of a policy quality based on OPTIMIZE info.
;;; ENVIRONMENT is global and used for system dispatch.
;;; NAME is the name of the quality.
;;; Should return the value.
(defgeneric compute-policy-quality (name optimize environment)
  (:argument-precedence-order name environment optimize))

;;; If a policy is directly specified, just use that.
(defmethod compute-policy-quality :around
    (name optimize environment)
  (multiple-value-bind (value present-p)
      (optimize-value optimize name)
    (if present-p
	value
	(call-next-method))))

(defmethod compute-policy-quality (name optimize environment)
  (declare (ignore optimize))
  (error 'no-policy-computer :quality name :env environment))

;;; Compute the entire policy for given OPTIMIZE info.
;;; ENVIRONMENT is global and used for system dispatch.
;;; This is a generic so that in the future an implementation could
;;; hypothetically override the whole process; however, doing so
;;; would take more understanding of POLICY objects than is
;;; presently external.
(defgeneric compute-policy (optimize environment)
  (:argument-precedence-order environment optimize))

;;; Default method for the usual case of the implementation not
;;; overriding the entire process.
(defmethod compute-policy (optimize environment)
  (let ((policy-qualities (policy-qualities environment))
	(optimize (normalize-optimize optimize environment)))
    ;; uses representation of policies as alists
    (loop for (name) in policy-qualities ; ignore CDR
	  collect (cons name (compute-policy-quality
			      name optimize environment)))))
