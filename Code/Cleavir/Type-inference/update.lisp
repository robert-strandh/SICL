(cl:in-package #:cleavir-type-inference)

(defun update (location type-descriptor bag)
  (cons (cons location type-descriptor)
	(remove location bag
		:test #'eq :key #'first)))

(defun find-type (location bag)
  (cdr (assoc location bag :test #'eq)))

(defun type-equal (type1 type2)
  (and (subtypep type1 type2) (subtypep type2 type1)))

;;; This function takes a Common Lisp type specifier that is
;;; guaranteed not to be type equal to NIL, checks whether it is type
;;; equal to one of the recognized type descriptors that we handle,
;;; and if so, returns that type descriptor.  Otherwise it returns
;;; NIL.
;;;
;;; FIXME: handle more types.
(defun canonicalize-type (type)
  (cond ((type-equal type 'fixnum) 'fixnum)
	((type-equal type 'null) 'null)
	((type-equal type 'cons) 'cons)
	((type-equal type 'short-float) 'short-float)
	((type-equal type 'single-float) 'single-float)
	((type-equal type 'double-float) 'double-float)
	((type-equal type 'long-float) 'long-float)
	(t nil)))
