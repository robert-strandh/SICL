(cl:in-package #:cleavir-generate-ast)

;;;; The purpose of the functions and the macro in this file is to
;;;; help handle source-tracking information.  The main entry point in
;;;; this file is the macro DB which is very similar to the standard
;;;; Common Lisp macro DESTRUCTURING-BIND.  First of all, DB is less
;;;; general than DESTRUCTURING-BIND in that it does not handle an
;;;; arbitrary lambda list; only a trees of variables, similar to the
;;;; destructuring done by LOOP.  Second, the FORM argument to DB can
;;;; be either an ordinary tree made up of conses and atoms, or it can
;;;; be an instance of SOURCE-LOCATION, in which case the
;;;; destructuring is done using the CHILDREN of that source location.
;;;; Third, DP takes an additional argument (the first one) compared
;;;; to DESTRUCTURING-BIND.  That argument is a variable that will be
;;;; bound to the LOCATION slot of the SOURCE-LOCATION object if the
;;;; FOR is an instance of SOURCE-LOCATION and to NIL if not.
;;;;
;;;; The idea is to use the DP macro for destructuring mainly in
;;;; methods on CONVERT-SPECIAL but also in other places where it is
;;;; convenient.  We can introduce the use of the DP macro in such
;;;; methods without modifying the arguments to CONVERT-SPECIAL.  When
;;;; all uses of destructuring using techniques other than DP have
;;;; been converted to DP, it should be straightforward to allow
;;;; instances of SOURCE-LOCATION to be passed as well.  Ultimately,
;;;; we might modify the DP macro so that only instances of
;;;; SOURCE-LOCATION are allowed.

(defgeneric raw (x))

(defmethod raw (x)
  x)

(defmethod raw ((x source-location))
  (expression x))

(defgeneric dfirst (x))

(defmethod dfirst (x)
  (error "invalid argument ~s" x))

(defmethod dfirst ((x cons))
  (first x))

(defmethod dfirst ((x source-location))
  (dfirst (children x)))

(defgeneric drest (x))

(defmethod drest (x)
  (error "invalid argument ~s" x))

(defmethod drest ((x cons))
  (rest x))

(defmethod drest ((x source-location))
  (make-instance 'source-location
    :expression (rest (expression x))
    :location (location x)
    :children (drest (children x))))

;;; This function generates code for destructuring a value according
;;; to a tree of variables.  TREE is a tree of variable names
;;; (symbols).  FORM is a form that, at runtime, computes the value to
;;; be assigned to the root of TREE.  This function returns a list of
;;; bindings to be used in a LET* form.  These bindings destructure
;;; the root value until the leaves of the tree are reached,
;;; generating intermediate temporary variables as necessary.  The
;;; destructuring code calls the function DFIRST and DREST so that an
;;; error is signaled whenever the corresponding place in the value
;;; tree is not a CONS cell, and so that source locations are handled
;;; as well as lists.
(defun destructure-variables (tree form)
  (let ((bindings '()))
    (labels ((traverse (tree form)
	       (cond ((null tree)
		      nil)
		     ((symbolp tree)
		      (push `(,tree ,form) bindings))
		     ((not (consp tree))
		      (error 'expectetree-but-found
			     :found tree))
		     (t
		      (let ((temp (gensym)))
			(push `(,temp ,form) bindings)
			(traverse (first tree) `(dfirst ,temp))
			(traverse (rest tree) `(drest ,temp)))))))
      (traverse tree form)
      (reverse bindings))))


(defmacro db (source-info-var tree form &body body)
  (let ((form-var (gensym)))
    `(let* ((,form-var ,form)
	    (,source-info-var (if (typep ,form-var 'source-location)
				  (location ,form-var)
				  nil))
	    ,@(destructure-variables tree form-var))
       (declare (ignorable ,source-info-var))
       ,@body)))
