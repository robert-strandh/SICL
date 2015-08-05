(cl:in-package #:cleavir-type-inference)

(defun update (location type-descriptor bag)
  (cons (cons location type-descriptor)
	(remove location bag
		:test #'eq :key #'first)))

(defun find-type (location bag)
  (cdr (assoc location bag :test #'eq)))
