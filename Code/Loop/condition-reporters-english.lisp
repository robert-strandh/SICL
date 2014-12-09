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

(defmethod report-loop-condition
    ((condition expected-var-spec-but-end) stream)
  (declare (ignorable condition))
  (format stream
	  "Expected a variable specification, but reached~@
           the end of the loop body."))

(defmethod report-loop-condition
    ((condition expected-var-spec-but-found) stream)
  (format stream
	  "Expected a variable specification but found~@
           the following instead:~@
           ~s"
	  (found condition)))

(defmethod report-loop-condition
    ((condition expected-simple-var-but-end) stream)
  (declare (ignorable condition))
  (format stream
	  "Expected a simple variable but reached~@
           the end of the loop body."))

(defmethod report-loop-condition
    ((condition expected-simple-var-but-found) stream)
  (format stream
	  "Expected a simple variable but found~@
           the following instead:~@
           ~s"
	  (found condition)))

(defmethod report-loop-condition
    ((condition expected-type-spec-but-end) stream)
  (declare (ignorable condition))
  (format stream
	  "Expected a variable specification but reached~@
           the end of the loop body."))

(defmethod report-loop-condition
    ((condition expected-type-spec-but-found) stream)
  (format stream
	  "Expected a type specification but found~@
           the following instead:~@
           ~s"
	  (found condition)))

(defmethod report-loop-condition
    ((condition expected-compound-form-but-end) stream)
  (declare (ignorable condition))
     (format stream
	     "Expected a compound form but reached ~
              the end of the loop body."))

(defmethod report-loop-condition
    ((condition expected-compound-form-but-found) stream)
  (format stream
	  "Expected a compound form but found~@
           the following instead:~@
           ~s"
	  (found condition)))

(defmethod report-loop-condition
    ((condition expected-form-but-end) stream)
  (declare (ignorable condition))
  (format stream
	  "Expected a form but reached~@
           the end of the loop body."))

(defmethod report-loop-condition
    ((condition expected-symbol-but-end) stream)
  (declare (ignorable condition))
  (format stream
	  "Expected a symbol but reached~@
           the end of the loop body."))

(defmethod report-loop-condition
    ((condition expected-symbol-but-found) stream)
  (format stream
	  "Expected a symbol but found~@
           the following instead:~@
           ~s"
	  (found condition)))

(defmethod report-loop-condition
    ((condition expected-keyword-but-found) stream)
  (format stream
	  "Expected a loop keyword, but found~@
           the following instead:~@
           ~s"
	  (found condition)))

(defmethod report-loop-condition
    ((condition expected-for/as-subclause-but-end) stream)
  (declare (ignorable condition))
  (format stream
	  "Expected a loop keyword indicating a for/as~@
           subclause, but reached the end of the loop body."))

(defmethod report-loop-condition
    ((condition expected-symbol-but-found) stream)
  (format stream
	  "Expected a loop keyword indicating a for/as~@
           subclause, but found the following instead:~@
           ~s"
	  (found condition)))

(defmethod report-loop-condition
    ((condition expected-each/the-but-end) stream)
  (declare (ignorable condition))
  (format stream
	  "Expected the loop keyword each/the,~@
           but reached the end of the loop body."))

(defmethod report-loop-condition
    ((condition expected-each/the-but-found) stream)
  (format stream
	  "Expected the loop keyword each/the, but found~@
           the following instead:~@
           ~s"
	  (found condition)))

(defmethod report-loop-condition
    ((condition expected-hash-or-package-but-end) stream)
  (declare (ignorable condition))
  (format stream
	  "Expected a loop keyword indicating a for/as-hash,~@
           but reached the end of the loop body."))

(defmethod report-loop-condition
    ((condition expected-hash-or-package-but-found) stream)
  (format stream
	  "Expected a loop keyword indicating a for/as-hash~@
           or a for/as-package subclause, but found~@
           the following instead:~@
           ~s"
	  (found condition)))

(defmethod report-loop-condition
    ((condition expected-in/of-but-end) stream)
  (declare (ignorable condition))
  (format stream
	  "Expected the loop keyword in/or,~@
           but reached the end of the loop body."))

(defmethod report-loop-condition
    ((condition expected-in/of-but-found) stream)
  (format stream
	  "Expected the loop keyword in/or, but found~@
           the following instead:~@
           ~s"
	  (found condition)))

(defmethod report-loop-condition
    ((condition expected-hash-key-but-end) stream)
  (declare (ignorable condition))
  (format stream
	  "Expected (hash-key other-var),~@
           but reached the end of the loop body."))

(defmethod report-loop-condition
    ((condition expected-hash-value-but-end) stream)
  (declare (ignorable condition))
  (format stream
	  "Expected (hash-value other-var),~@
           but reached the end of the loop body."))

(defmethod report-loop-condition
    ((condition expected-hash-key-but-found) stream)
  (format stream
	  "Expected (hash-key other-var), but found~@
           the following instead:~@
           ~s"
	  (found condition)))

(defmethod report-loop-condition
    ((condition expected-hash-value-but-found) stream)
  (format stream
	  "Expected (hash-value other-var), but found~@
           the following instead:~@
           ~s"
	  (found condition)))

(defmethod report-loop-condition
    ((condition expected-preposition-but-end) stream)
  (declare (ignorable condition))
  (format stream
	  "Expected a for/as preposition,~@
           but reached the end of the loop body."))

(defmethod report-loop-condition
    ((condition too-many-prepositions-from-one-group) stream)
  (format stream
	  "Expected (hash-value other-var), but found~@
           the following instead:~@
           ~s"
	  (found condition)))

(defmethod report-loop-condition
    ((condition conflicting-stepping-directions) stream)
  (declare (ignorable condition))
  (format stream
	  "Conflicting stepping directions."))
