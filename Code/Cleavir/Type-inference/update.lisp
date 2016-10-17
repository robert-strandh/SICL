(cl:in-package #:cleavir-type-inference)

(defun update (location type-descriptor bag)
  (cons (cons location type-descriptor)
	(remove location bag
		:test #'eq :key #'first)))

;;; if you are seeing this error and looking for the cause:
;;; compute-initial-dictionary should give every location a type of
;;;  T in every arc that location is live in. It is not doing so.
(define-condition type-missing (error)
  ((location :initarg :location :reader type-missing-location)
   (bag :initarg :bag :reader type-missing-bag))
  (:report
   (lambda (condition stream)
     (format stream "No type information for location ~s in bag ~s.
This is probably an internal bug in type inference (or related systems, e.g. liveness)."
             (type-missing-location condition)
             (type-missing-bag condition)))))

(defgeneric find-type (location bag)
  (:method (location bag)
    (let ((a (assoc location bag :test #'eq)))
      (if a
          (cdr a)
          (error 'type-missing :location location :bag bag)))))
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
