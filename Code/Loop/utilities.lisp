;;;; Copyright (c) 2008, 2009, 2010, 2011, 2012, 2013, 2014
;;;;
;;;;     Robert Strandh (robert.strandh@gmail.com)
;;;;
;;;; all rights reserved. 
;;;;
;;;; Permission is hereby granted to use this software for any 
;;;; purpose, including using, modifying, and redistributing it.
;;;;
;;;; The software is provided "as-is" with no warranty.  The user of
;;;; this software assumes any responsibility of the consequences. 

(cl:in-package #:sicl-loop)

;;; Loop keywords are symbols, but they are not recognized by symbol
;;; identity as is usually the case, but instead by their names.  The
;;; HyperSpec doesn't say what function to use for comparing the
;;; names.  We assume string= here, meaning that the names are case 
;;; sensitive. 

(defun symbol-equal (symbol1 symbol2)
  (and (symbolp symbol1)
       (string= symbol1 symbol2)))

;;; This function generates code for destructuring a value according
;;; to a tree of variables.  D-VAR-SPEC is a tree of variable names
;;; (symbols).  FORM is a form that, at runtime, computes the value to
;;; be assigned to the root of D-VAR-SPEC.  This function returns a
;;; list of bindings to be used in a LET* form.  These bindings
;;; destructure the root value until the leaves of the tree are
;;; reached, generating intermediate temporary variables as necessary.
;;; The destructuring code calls the function LIST-CAR and LIST-CDR so
;;; that an error is signaled whenever the corresponding place in the
;;; value tree is not a CONS cell.
(defun destructure-variables (d-var-spec form)
  (let ((bindings '()))
    (labels ((traverse (d-var-spec form)
	       (cond ((null d-var-spec)
		      nil)
		     ((symbolp d-var-spec)
		      (push `(,d-var-spec ,form) bindings))
		     ((not (consp d-var-spec))
		      (error 'expected-var-spec-but-found
			     :found d-var-spec))
		     (t
		      (let ((temp (gensym)))
			(push `(,temp ,form) bindings)
			(traverse (car d-var-spec) `(list-car ,temp))
			(traverse (cdr d-var-spec) `(list-cdr ,temp)))))))
      (traverse d-var-spec form)
      (reverse bindings))))

;;; Given a D-VAR-SPEC, compute a D-VAR-SPEC with the same structure
;;; as the one given as argument, except that the non-NIL leaves
;;; (i.e., the variables names) have been replaced by fresh symbols.
;;; Return two values: the new D-VAR-SPEC and a dictionary in the form
;;; of an association list that gives the correspondence between the
;;; original and the new variables.
(defun fresh-variables (d-var-spec)
  (let* ((dictionary '()))
    (labels ((traverse (d-var-spec)
	       (cond ((null d-var-spec)
		      nil)
		     ((symbolp d-var-spec)
		      (let ((temp (gensym)))
			(push (cons d-var-spec temp) dictionary)
			temp))
		     (t
		      (cons (traverse (car d-var-spec))
			    (traverse (cdr d-var-spec)))))))
      (values (traverse d-var-spec)
	      (reverse dictionary)))))

;;; ASSIGNMENTS is a list of pairs.  Each pair is a CONS cell.  The
;;; CAR of the CONS cell is a D-VAR-SPEC and the CDR of the CONS cell
;;; is a form to be destructured according to the corresponding
;;; D-VAR-SPEC.  These destructurings have to be done in parallel, as
;;; when the loop keyword AND was given.  This function returns a list
;;; of bindings to be used with a single LET* form that accomplishes
;;; these destructurings.  The technique we use is to first generate
;;; fresh variables and do the assignments to them, and only at the
;;; end, assign the original variables to the fresh ones.
(defun compute-bindings (assignments)
  (let* ((temps
	   (loop for (d-var-spec) in assignments
		 collect (multiple-value-list (fresh-variables d-var-spec))))
	 (initial-bindings
	   (loop for (d-var-spec) in temps
		 for (nil . form) in assignments
		 append (destructure-variables d-var-spec form)))
	 (final-bindings
	   (loop for (nil dictionary) in temps
		 append (loop for (var . val) in dictionary
			      collect `(,var ,val)))))
    (append initial-bindings final-bindings)))

;;; Extract variables
(defun extract-variables (d-var-spec d-type-spec)
  (let ((result '()))
    (labels ((extract-aux (d-var-spec d-type-spec)
	       (cond ((null d-var-spec)
		      nil)
		     ((symbolp d-var-spec)
		      (push (list d-var-spec (or d-type-spec t)) result))
		     ((symbolp d-type-spec)
		      (if (not (consp d-var-spec))
			  (error 'expected-var-spec-but-found
				 :found d-var-spec)
			  (progn (extract-aux (car d-var-spec) d-type-spec)
				 (extract-aux (cdr d-var-spec) d-type-spec))))
		     ((not (consp d-var-spec))
		      (error 'expected-var-spec-but-found
			     :found d-var-spec))
		     ((not (consp d-type-spec))
		      (error 'expected-type-spec-but-found
			     :found d-type-spec))
		     (t
		      (extract-aux (car d-var-spec) (car d-type-spec))
		      (extract-aux (cdr d-var-spec) (cdr d-type-spec))))))
      (extract-aux d-var-spec d-type-spec)
      result)))
