(cl:in-package :sicl-conditionals)

(defmethod acclimation:report-condition ((condition malformed-cond-clause)
                                          stream
                                          (language acclimation:english))
  (format stream
          "Expected a cond clause of the form,~@
           (test-form form*),~@
           but the following was given instead:~@
           ~s"
          (clause condition)))

(defmethod acclimation:report-condition ((condition malformed-case-clauses)
                                          stream
                                          (language acclimation:english))
  (format stream
          "Expected a proper list of case clauses,~@
           but the following was given instead:~@
           ~s"
          (clauses condition)))

(defmethod acclimation:report-condition ((condition malformed-case-clause)
                                          stream
                                          (language acclimation:english))
  (format stream
          "Expected a case clause of the form,~@
           (keys form*),~@
           but the following was given instead:~@
           ~s"
          (clause condition)))

(defmethod acclimation:report-condition ((condition otherwise-clause-not-last)
                                          stream
                                          (language acclimation:english))
  (format stream
          "The `otherwise' or `t' clause must be last in a case form,~@
           but but it was followed by:~@
           ~s"
          (clauses condition)))

(defmethod acclimation:report-condition ((condition malformed-keys)
                                          stream
                                          (language acclimation:english))
  (format stream
          "Expected a designator for a list of keys,~@
           but the following was given instead:~@
           ~s"
          (keys condition)))

(defmethod acclimation:report-condition ((condition malformed-typecase-clauses)
                                          stream
                                          (language acclimation:english))
  (format stream
          "Expected a proper list of typecase clauses,~@
           but the following was given instead:~@
           ~s"
          (clauses condition)))

(defmethod acclimation:report-condition ((condition malformed-typecase-clause)
                                          stream
                                          (language acclimation:english))
  (format stream
          "Expected a typecase clause of the form,~@
          (type form*),~@
           but the following was given instead:~@
           ~s"
          (clause condition)))
