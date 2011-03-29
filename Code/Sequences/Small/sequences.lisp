(in-package #:sicl-sequences-small)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conditions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Internal conditions

(define-condition must-be-proper-list-internal (type-error) ())
  
(define-condition invalid-sequence-index-internal (type-error)
  ((%sequence-index :initarg sequence-index :reader sequence-index)))

(define-condition invalid-start-index-internal (invalid-sequence-index-internal)
  ())

(define-condition invalid-end-index-internal (invalid-sequence-index-internal)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; External conditions

(define-condition type-error-with-detecting-function (type-error)
  ((%detecting-function :initarg :detecting-function
			:reader detecting-function)))

(define-condition must-be-proper-list (type-error-with-detecting-function)
  ())

(define-condition invalid-sequence-index (type-error-with-detecting-function)
  ((%sequence-index :initarg sequence-index :reader sequence-index)))

(define-condition invalid-start-index (invalid-sequence-index) ())

(define-condition invalid-end-index (invalid-sequence-index) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Wrapper for sequence functions that takes care of 
;;; signaling errors. 

(defun call-with-handlers-bound (thunk function-name)
  (handler-bind ((must-be-proper-list-internal
		  (lambda (condition)
		    (error 'must-be-proper-list
			   :detecting-function function-name
			   :offending-datum (type-error-datum condition)
			   :expected-type 'proper-list)))
		 (invalid-start-index-internal
		  (lambda (condition)
		    (error 'invalid-start-index
			   :detecting-function function-name
			   :offending-datum (type-error-datum condition)
			   :expected-type (type-error-expected-type condition)
			   :sequence-index (sequence-index condition))))
		 (invalid-end-index-internal
		  (lambda (condition)
		    (error 'invalid-end-index
			   :detecting-function function-name
			   :offending-datum (type-error-datum condition)
			   :expected-type (type-error-expected-type condition)
			   :sequence-index (sequence-index condition))))
    (funcall thunk)))

(defmacro with-handlers-bound (function-name &body body)
  `(call-with-handlers-bound (lambda () ,@body) ,function-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utility functions

;;; Skip a prefix of a certain length, and check that the list 
;;; doesn't end with an atom, and that it is at least as long as
;;; the prefix length.  
(defun skip-prefix (list prefix-length)
  (loop with remaining = list
	with remaining-length = prefix-length
	while (consp remaining)
	while (plusp remaining-prefix-length)
	do (setf remaining (cdr remaining))
	   (decf remaining-prefix-length))
  (cond ((and (atom remaining) (not (null remaining)))
	 (error 'must-be-proper-list-internal
		:offending-datum list
		:expected-type 'proper-list))
	((plusp remaining-length)
	 (error 'invalid-start-index
		:offending-datum list
		:expected-type `(integer 0 (,(- prefix-length remaining-length)))))
	(t
	 remaining)))

(defun |apply-to-elements seq-type=list end=nil|
    (list start function)
  (let ((remaining (skip-prefix list prefix-length)))
    (loop while (consp remaining)
	  do (funcall function (car remaining))
	     (setf remaining (cdr remaining)))
    (when (atom remaining)
      (error 'must-be-proper-list-internal
	     :offending-datum list
	     :expected-type 'proper-list))))

(defun |apply-to-elements seq-type=list end=other|
    (list start end function)
  (let ((remaining (skip-prefix list start))
	(end-start (- end start)))
    (loop while (consp remaining)
	  while (plusp end-start)
	  do (funcall function (car remaining))
	     (setf remaining (cdr remaining))
	     (decf end-start))
    (when (and (atom remaining) (not (null remaining)))
      (error 'must-be-proper-list-internal
	     :offending-datum list
	     :expected-type 'proper-list))
    (when (plusp end-start)
      (error 'invalid-end-index-internal
	     :offending-datum end
	     :expected-type `(integer 0 ,(- end end-start))))))

(defun |apply-to-elements seq-type=vector|
    (vector start end function)
  (let ((length (length vector)))
    (when 
	 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function count-if

(defun |count-if seq-type=list end=nil from-end=nil|
    (predicate list start key)
  (let ((count 0))
    (apply-to-remaining-elements
     list
     start
     (lambda (element)
       (when (funcall predicate (funcall key element))
	 (incf count))))
    count))