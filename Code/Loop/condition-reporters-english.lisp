(cl:in-package #:sicl-loop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Condition reporters for parse errors.

(defmethod acclimation:report-condition
    ((condition expected-var-spec-but-end)
     stream
     (language acclimation:english))
  (declare (ignorable condition))
  (format stream
          "Expected a variable specification, but reached~@
           the end of the loop body."))

(defmethod acclimation:report-condition
    ((condition expected-var-spec-but-found)
     stream
     (language acclimation:english))
  (format stream
          "Expected a variable specification but found~@
           the following instead:~@
           ~s"
          (found condition)))

(defmethod acclimation:report-condition
    ((condition expected-simple-var-but-end)
     stream
     (language acclimation:english))
  (declare (ignorable condition))
  (format stream
          "Expected a simple variable but reached~@
           the end of the loop body."))

(defmethod acclimation:report-condition
    ((condition expected-simple-var-but-found)
     stream
     (language acclimation:english))
  (format stream
          "Expected a simple variable but found~@
           the following instead:~@
           ~s"
          (found condition)))

(defmethod acclimation:report-condition
    ((condition expected-type-spec-but-end)
     stream
     (language acclimation:english))
  (declare (ignorable condition))
  (format stream
          "Expected a variable specification but reached~@
           the end of the loop body."))

(defmethod acclimation:report-condition
    ((condition expected-type-spec-but-found)
     stream
     (language acclimation:english))
  (format stream
          "Expected a type specification but found~@
           the following instead:~@
           ~s"
          (found condition)))

(defmethod acclimation:report-condition
    ((condition expected-compound-form-but-end)
     stream
     (language acclimation:english))
  (declare (ignorable condition))
     (format stream
             "Expected a compound form but reached ~
              the end of the loop body."))

(defmethod acclimation:report-condition
    ((condition expected-compound-form-but-found)
     stream
     (language acclimation:english))
  (format stream
          "Expected a compound form but found~@
           the following instead:~@
           ~s"
          (found condition)))

(defmethod acclimation:report-condition
    ((condition expected-form-but-end)
     stream
     (language acclimation:english))
  (declare (ignorable condition))
  (format stream
          "Expected a form but reached~@
           the end of the loop body."))

(defmethod acclimation:report-condition
    ((condition expected-symbol-but-end)
     stream
     (language acclimation:english))
  (declare (ignorable condition))
  (format stream
          "Expected a symbol but reached~@
           the end of the loop body."))

(defmethod acclimation:report-condition
    ((condition expected-symbol-but-found)
     stream
     (language acclimation:english))
  (format stream
          "Expected a symbol but found~@
           the following instead:~@
           ~s"
          (found condition)))

(defmethod acclimation:report-condition
    ((condition expected-keyword-but-found)
     stream
     (language acclimation:english))
  (format stream
          "Expected a loop keyword, but found~@
           the following instead:~@
           ~s"
          (found condition)))

(defmethod acclimation:report-condition
    ((condition expected-for/as-subclause-but-end)
     stream
     (language acclimation:english))
  (declare (ignorable condition))
  (format stream
          "Expected a loop keyword indicating a for/as~@
           subclause, but reached the end of the loop body."))

(defmethod acclimation:report-condition
    ((condition expected-symbol-but-found)
     stream
     (language acclimation:english))
  (format stream
          "Expected a loop keyword indicating a for/as~@
           subclause, but found the following instead:~@
           ~s"
          (found condition)))

(defmethod acclimation:report-condition
    ((condition expected-each/the-but-end)
     stream
     (language acclimation:english))
  (declare (ignorable condition))
  (format stream
          "Expected the loop keyword each/the,~@
           but reached the end of the loop body."))

(defmethod acclimation:report-condition
    ((condition expected-each/the-but-found)
     stream
     (language acclimation:english))
  (format stream
          "Expected the loop keyword each/the, but found~@
           the following instead:~@
           ~s"
          (found condition)))

(defmethod acclimation:report-condition
    ((condition expected-hash-or-package-but-end)
     stream
     (language acclimation:english))
  (declare (ignorable condition))
  (format stream
          "Expected a loop keyword indicating a for/as-hash,~@
           but reached the end of the loop body."))

(defmethod acclimation:report-condition
    ((condition expected-hash-or-package-but-found)
     stream
     (language acclimation:english))
  (format stream
          "Expected a loop keyword indicating a for/as-hash~@
           or a for/as-package subclause, but found~@
           the following instead:~@
           ~s"
          (found condition)))

(defmethod acclimation:report-condition
    ((condition expected-in/of-but-end)
     stream
     (language acclimation:english))
  (declare (ignorable condition))
  (format stream
          "Expected the loop keyword in/or,~@
           but reached the end of the loop body."))

(defmethod acclimation:report-condition
    ((condition expected-in/of-but-found)
     stream
     (language acclimation:english))
  (format stream
          "Expected the loop keyword in/or, but found~@
           the following instead:~@
           ~s"
          (found condition)))

(defmethod acclimation:report-condition
    ((condition expected-hash-key-but-end)
     stream
     (language acclimation:english))
  (declare (ignorable condition))
  (format stream
          "Expected (hash-key other-var),~@
           but reached the end of the loop body."))

(defmethod acclimation:report-condition
    ((condition expected-hash-value-but-end)
     stream
     (language acclimation:english))
  (declare (ignorable condition))
  (format stream
          "Expected (hash-value other-var),~@
           but reached the end of the loop body."))

(defmethod acclimation:report-condition
    ((condition expected-hash-key-but-found)
     stream
     (language acclimation:english))
  (format stream
          "Expected (hash-key other-var), but found~@
           the following instead:~@
           ~s"
          (found condition)))

(defmethod acclimation:report-condition
    ((condition expected-hash-value-but-found)
     stream
     (language acclimation:english))
  (format stream
          "Expected (hash-value other-var), but found~@
           the following instead:~@
           ~s"
          (found condition)))

(defmethod acclimation:report-condition
    ((condition expected-preposition-but-end)
     stream
     (language acclimation:english))
  (declare (ignorable condition))
  (format stream
          "Expected a for/as preposition,~@
           but reached the end of the loop body."))

(defmethod acclimation:report-condition
    ((condition too-many-prepositions-from-one-group)
     stream
     (language acclimation:english))
  (format stream
          "Expected (hash-value other-var), but found~@
           the following instead:~@
           ~s"
          (found condition)))

(defmethod acclimation:report-condition
    ((condition conflicting-stepping-directions)
     stream
     (language acclimation:english))
  (declare (ignorable condition))
  (format stream
          "Conflicting stepping directions."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Condition reporters for syntax errors.

(defmethod acclimation:report-condition
    ((condition name-clause-not-first)
     stream
     (language acclimation:english))
  (declare (ignorable condition))
  (format stream
          "A NAME loop clause was found, but it was~@
           not the first clause."))

(defmethod acclimation:report-condition
    ((condition multiple-name-clauses)
     stream
     (language acclimation:english))
  (declare (ignorable condition))
  (format stream
          "Multiple NAME clauses where found."))

(defmethod acclimation:report-condition
    ((condition invalid-clause-order)
     stream
     (language acclimation:english))
  (declare (ignorable condition))
  (format stream
          "Invalid clause order.  Variable clauses must precede main clauses."))

(defmethod acclimation:report-condition
    ((condition multiple-variable-occurrences)
     stream
     (language acclimation:english))
  (format stream
          "Multiple occurrences of the following variable were found:~@
           ~s"
          (bound-variable condition)))
