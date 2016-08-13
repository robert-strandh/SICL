(cl:in-package #:sicl-sequence)

;;; Skip a prefix of a list and signal an error if the list is too
;;; short, or if it is not a proper list.  Also check that start is a
;;; nonnegative integer.
(defun skip-to-start (name list start)
  (let ((start-bis start)
	(remaining list))
    (loop until (zerop start-bis)
	  until (atom remaining)
	  do (setf remaining (cdr remaining))
	     (decf start-bis)
	  finally (when (and (atom remaining) (not (null remaining)))
		    (error 'must-be-proper-list
			   :name name
			   :datum list))
		  (when (plusp start-bis)
		    (error 'invalid-start-index
			   :name name
			   :datum start
			   :expected-type `(integer 0 ,(- start start-bis))
			   :in-sequence list)))
    remaining))

;;; This function is called at the end of some list traversal
;;; to make sure that the list is a proper list.
(defun tail-must-be-proper-list (name list tail)
  (when (and (atom tail) (not (null tail)))
    (error 'must-be-proper-list
	   :name name
	   :datum list)))

;;; This function is called at the end of some list traversal
;;; to make sure that the list is a proper list, and to make sure
;;; that end is a valid index.
(defun tail-must-be-proper-list-with-end (name list tail end length)
  (when (and (atom tail) (not (null tail)))
    (error 'must-be-proper-list
	   :name name
	   :datum list))
  (when (and (atom tail) (< length end))
    (error 'invalid-end-index
	   :name name
	   :datum end
	   :in-sequence list
	   :expected-type `(integer 0 ,length))))

;;; This function is used when the sequence is a vector of some kind
;;; in order to verify that start and end are valid bounding indexes.
;;; It has already been verified that start is a nonnegative integer.
;;; FIXME: What do we know about end?
(defun verify-bounding-indexes (name vector start end)
  (let ((length (length vector)))
    (when (> start length)
      (error 'invalid-start-index
	     :name name
	     :datum start
	     :expected-type `(integer 0 ,length)
	     :in-sequence vector))
    (unless (<= 0 end length)
      (error 'invalid-end-index
	     :name name
	     :datum end
	     :expected-type `(integer 0 ,length)
	     :in-sequence vector))
    (unless (<= start end)
      (error 'end-less-than-start
	     :name name
	     :datum start
	     :expected-type `(integer 0 ,end)
	     :end-index end
	     :in-sequence vector))))

;;; This function is used to compute the length of the list
;;; given a remainder and a start index.
(defun compute-length-from-remainder (name list remainder start)
  (loop for length from start
	until (atom remainder)
	do (setf remainder (cdr remainder))
	finally (unless (null remainder)
		  (error 'must-be-proper-list
			 :name name
			 :datum list))
		(return length)))

;;; This function is used to verify that the end sequence index
;;; is valid, and that, if we reach the end of the list, it is 
;;; a proper list.  The remainder of the list is returned.
(defun verify-end-index (name list remainder start end)
  (loop for length from start
	until (or (atom remainder) (>= length end))
	do (setf remainder (cdr remainder))
	   finally (unless (or (null remainder) (consp remainder))
		     (error 'must-be-proper-list
			 :name name
			 :datum list))
		   (when (< length end)
		     (error 'invalid-end-index
			    :name name
			    :datum end
			    :expect-type `(integer 0 ,length)
			    :in-sequence list))
		   (return remainder)))

(defmacro process-list-elements-from-start-without-end
    ((list-form cons-var) &body body)
  `(loop for ,cons-var = ,list-form then (cdr ,cons-var)
	 while (consp ,cons-var)
	 do ,@body
	 finally (unless (null ,cons-var)
		   (error "Not a proper list"))))


;;; This function calls KEY-FUNCTION with ELEMENT as the only
;;; argument.  It is semantically equivalent to FUNCALL.  However, we
;;; do not trust the implementation of FUNCALL to optimize when
;;; KEY-FUNCTION is either #'IDENTITY, #'CAR, or #'CDR, so we treat
;;; those cases separately here.
;;;
;;; When this function is inlined in a context where the key-function
;;; is EQ to one of the constants tested for in the function body, a
;;; sufficently smart compiler will remove the test and just leave the
;;; appropriate branch.

(declaim (inline apply-key-function))

(defun apply-key-function (element key-function)
  (declare (optimize (speed 3) (debug 0) (safety 3)))
  (cond ((or (eq key-function #'identity)
	     (eq key-function 'identity))
	 element)
	((or (eq key-function #'car)
	     (eq key-function 'car))
	 (car element))
	((or (eq key-function #'cdr)
	     (eq key-function 'cdr))
	 (cdr element))
	(t
	 (funcall key-function element))))
