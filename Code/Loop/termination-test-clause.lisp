(cl:in-package #:sicl-loop)

;;; We define a class that is the root class of all termination-test
;;; clauses.  Recall that a termination-test clause is a main clause,
;;; and that the HyperSpec defines TERMINATION-TEST as follows:
;;;
;;;   termination-test ::= while form |
;;;                        until form |
;;;                        repeat form |
;;;                        always form |
;;;                        never form |
;;;                        thereis form

(defclass termination-test-clause (main-clause) ())

;;; The termination-test clauses do not bind any variables.
(defmethod bound-variables ((clause termination-test-clause))
  '())

(defmethod accumulation-variables ((clause termination-test-clause))
  '())
