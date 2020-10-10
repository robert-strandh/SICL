(cl:in-package #:sicl-loop)

;;; In the dictionary entry for LOOP, the HyperSpec says:
;;;
;;;   variable-clause ::= with-clause | initial-final | for-as-clause
;;;
;;; Here, we exclude initial-final.  The reason for that is that
;;; initial-final is also one of the possibilities for a
;;; main-clause, and the reason for this "multiple inheritance" is
;;; so that the LOOP macro syntax can be defined to have the syntax:
;;;
;;;   loop [name-clause] {variable-clause}* {main-clause}*
;;;
;;; which then allows for INITIALLY and FINALLY clauses to occur
;;; anywhere after the name-clause.
;;;
;;; What we do here is to treat INITIALLY and FINALLY specially, so
;;; that they are neither main clauses nor variable clauses.
;;; Therefore, here, we have:
;;;
;;;   variable-clause ::= with-clause | for-as-clause

(defclass variable-clause (clause) ())

;;; No variable clause defines any accumulation variables
(defmethod accumulation-variables ((clause variable-clause))
  '())
