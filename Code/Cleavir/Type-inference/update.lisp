(cl:in-package #:cleavir-type-inference)

(defun update (location type-descriptor bag)
  (cons (cons location type-descriptor)
	(remove location bag
		:test #'eq :key #'first)))

(defgeneric find-type (location bag)
  (:method (location bag)
    (cdr (assoc location bag :test #'eq))))
(defmethod find-type
    ((location cleavir-ir:constant-input) bag)
  (declare (ignore bag))
  (approximate-type `(eql ,(cleavir-ir:value location))))
(defmethod find-type
    ((location cleavir-ir:immediate-input) bag)
  (declare (ignore bag))
  (approximate-type `(eql ,(cleavir-ir:value location))))
(defmethod find-type
    ((location cleavir-ir:load-time-value-input) bag)
  (declare (ignore bag))
  ;; FIXME: obviously nonideal, but i don't want to think about
  ;;  non-eql values and so forth.
  (if (and (cleavir-ir:read-only-p location)
	   (consp (cleavir-ir:form location))
	   (eq (first (cleavir-ir:form location)) 'quote))
      (approximate-type
       `(eql ,(second (cleavir-ir:form location))))
      (call-next-method)))
