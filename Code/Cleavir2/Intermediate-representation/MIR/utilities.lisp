(cl:in-package #:cleavir-ir)

;;; For many of the instruction, client code has the choice between
;;; giving a list of inputs, outputs, or successors, and giving them
;;; as two keyword arguments.  The following utilities check that it
;;; is not the case that both a list and individual keyword arguments
;;; are supplied.  It also checks that, if a list was not given, both
;;; individual keyword arguments was provided.

(defun construct-inputs (i i-p i1 i1-p i2 i2-p)
  (cond (i-p
	 (when  (or i1-p i2-p)
	   (error 'input-inputs-mutually-exclusive))
	 i)
	((or (not i1-p) (not i2-p))
	 (error 'both-individual-inputs-must-be-given))
	(t
	 (list i1 i2))))

(defun construct-output (o o-p)
  (if (not o-p)
      (error 'output-must-be-given)
      (list o)))

(defun construct-successors (s s-p s1 s1-p s2 s2-p)
  (cond (s-p
	 (when  (or s1-p s2-p)
	   (error 'successor-successors-mutually-exclusive))
	 s)
	((or (and s1-p (not s2-p))
	     (and (not s1-p) s2-p))
	 (error 'both-or-no-individual-successors-must-be-given))
	(t
	 (list s1 s2))))

(defun construct-successors-bis (s s-p s1 s1-p s2 s2-p)
  (cond (s-p
	 (when  (or s1-p s2-p)
	   (error 'successor-successors-mutually-exclusive))
	 s)
	((and (not s1-p) s2-p)
	 (error 'successor1-must-be-given))
	(s2-p
	 (list s1 s2))
	(t
	 (list s1))))
