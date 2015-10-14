(cl:in-package #:cleavir-generate-ast)

;;; These two generic functions are called by Cleavir in order to
;;; obtain source information from the implementation.  The
;;; SOURCE-INFO argument is the object that the implementation passed
;;; to GENERATE-AST.  
;;;
;;; The minimum amount of customization required by an implementation
;;; that wants source tracking is to define a method on BEGIN-SOURCE,
;;; specialized to the particular class of its SOURCE-INFO object. 
;;;
;;; The method on BEGIN-SOURCE should return some object that the
;;; implementation uses in order to indicate source location, or NIL
;;; if no information is available on the location of EXPRESSION.  
;;;
;;; Cleavir will call these generic functions according to the nesting
;;; of the sub-expressions of the form that was passed to
;;; GENERATE-AST, so that a call to BEGIN-SOURCE with EXPRESSION
;;; indicates that subsequent calls to BEGIN-SOURCE will be with
;;; sub-expressions of EXPRESSION.  When Cleavir has obtained
;;; information about all sub-expressions of some expression, it calls
;;; END-SOURCE to inform the implementation of this fact.
;;;
;;; An implementation that simply maintains a hash table mapping
;;; compound expressions to source locations does not have to provide
;;; a method on END-SOURCE.  It would then just return the value in
;;; the hash table associated with the EXPRESSION key.  A more
;;; sophisticated implementation that maintains a tree of expressions
;;; would also provide a method on END-SOURCE so as to be informed
;;; when Cleavir has obtained all information about some expression.

(defgeneric begin-source (source-info expression))

(defmethod begin-source (source-info expression)
  (declare (ignore source-info expression))
  nil)

(defgeneric end-source (source-info))

(defmethod end-source (source-info)
  (declare (ignore source-info))
  nil)

;;; This function returns the location of an object.  If the argument
;;; is an instance of SOURCE-LOCATION, then the corresponding
;;; client-supplied location information is returned.  Otherwise, NIL
;;; is returned.
(defgeneric location (object))

;;; Data structure for source tracking.
(defclass source-location ()
  ((%expression :initarg :expression :reader expression)
   (%location :initarg :location :reader location)
   (%children :initarg :children :reader children)))

(defmethod location (object)
  (declare (ignore object))
  nil)

(defun build-source-info (source-info expression)
  (let ((location (begin-source source-info expression)))
    (unless (null location)
      (make-instance 'source-location
	:expression expression
	:location location
	:children
	(if (atom expression)
	    '()
	    (let ((result '()))
	      (loop for sub-expressions = expression
		      then (rest sub-expressions)
		    while (consp sub-expressions)
		    do (push (build-source-info source-info (first sub-expressions))
			     result)
		    finally (unless (null sub-expressions)
			      (push (build-source-info source-info sub-expressions)
				    result)))
	      (nreverse result)))))
    (end-source source-info)
    location))
