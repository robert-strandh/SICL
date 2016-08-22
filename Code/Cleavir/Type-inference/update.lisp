(cl:in-package #:cleavir-type-inference)

(defun update (location type-descriptor bag)
  (cons (cons location type-descriptor)
	(remove location bag
		:test #'eq :key #'first)))

(defgeneric find-type (location bag)
  (:method (location bag)
    (cdr (assoc location bag :test #'eq))))
(defmethod find-type ((location cleavir-ir:constant-input) bag)
  (declare (ignore bag))
  `(eql ,(cleavir-ir:value location)))
(defmethod find-type ((location cleavir-ir:immediate-input) bag)
  (declare (ignore bag))
  `(eql ,(cleavir-ir:value location)))
(defmethod find-type
    ((location cleavir-ir:load-time-value-input) bag)
  (declare (ignore bag))
  ;; FIXME: obviously nonideal, but i don't want to think about
  ;;  non-eql values and so forth.
  (if (and (cleavir-ir:read-only-p location)
	   (consp (cleavir-ir:form location))
	   (eq (first (cleavir-ir:form location)) 'quote))
      `(eql ,(second (cleavir-ir:form location)))
      (call-next-method)))

(defun type-equal (type1 type2)
  (and (subtypep type1 type2) (subtypep type2 type1)))

(defun top-p (type)
  (subtypep t type))

(defun bottom-p (type)
  (subtypep type nil))

;;; This function takes a Common Lisp type specifier that is
;;; guaranteed not to be type equal to NIL, checks whether it is type
;;; equal to one of the recognized type descriptors that we handle,
;;; and if so, returns that type descriptor.  Otherwise it returns
;;; NIL.
;;;
;;; FIXME: handle more types.
(defun canonicalize-type (type)
  (cond ((and (consp type) (eq (first type) 'eql)) type)
	((type-equal type 'fixnum) 'fixnum)
	((type-equal type 'null) 'null)
	((type-equal type 'cons) 'cons)
	((type-equal type 'short-float) 'short-float)
	((type-equal type 'single-float) 'single-float)
	((type-equal type 'double-float) 'double-float)
	((type-equal type 'long-float) 'long-float)
	((bottom-p type) nil)
	(t t)))

(defun as-values (type)
  (if (and (consp type) (eq (first type) 'values))
      type
      `(values ,type)))

(defun values-type-nth-forgiving (n values-type)
  (or (values-type-nth n values-type) 'null))

(defun values-type-nth (n values-type)
  (loop with i = 0
	for (current . rest) on (rest values-type)
	when (eq current '&rest)
	       ;; count no longer matters
	       do (return (first rest))
	unless (eq current '&optional)
	  when (= i n) do (return current)
	    else do (incf i)
	finally (return nil)))
