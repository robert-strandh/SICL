(cl:in-package #:cleavir-hir-transformations)

;;;; This transformation replaces every LOAD-TIME-VALUE-INPUT in the
;;;; program by a lexical location.  A form is appended to the forms
;;;; of the TOP-LEVEL-ENTER-INSTRUCTION for each LOAD-TIME-VALUE-INPUT
;;;; and the lexical location is added to the outputs of the initial
;;;; TOP-LEVEL-ENTER-INSTRUCTION.
;;;;
;;;; If the LOAD-TIME-VALUE-INPUT to be replaced has a form of type
;;;; (QUOTE constant), then CONVERT-CONSTANT-TO-IMMEDIATE is called,
;;;; passing the constant and the SYSTEM as arguments.  If that call
;;;; returns an integer, then instead of adding a form to the initial
;;;; instruction, this transformation replaces the input by an
;;;; immediate input instead.  If that call returns NIL, the normal
;;;; transformation is performed.
;;;;
;;;; Client code can add a method on CONVERT-CONSTANT-TO-IMMEDIATE
;;;; that should then specialize on the second parameter, i.e. the
;;;; SYSTEM.

(defgeneric convert-constant-to-immediate (constant system))

(defmethod convert-constant-to-immediate (constant system)
  (declare (ignore constant system))
  nil)

(defun replace-load-time-value-with-lexical (initial-instruction input)
  (setf (cleavir-ir:forms initial-instruction)
	(append (cleavir-ir:forms initial-instruction)
		(list (cleavir-ir:form input))))
  (setf (cleavir-ir:outputs initial-instruction)
	(append (cleavir-ir:outputs initial-instruction)
		(list input)))
  (change-class input 'cleavir-ir:lexical-location
		:name (gensym)))

(defun replace-load-time-value-with-immediate (value input)
  (change-class input 'cleavir-ir:immediate-input :value value))

(defun eliminate-load-time-value-input (initial-instruction input system)
  (let ((form (cleavir-ir:form input)))
    (if (and (consp form)
	     (consp (rest form))
	     (null (rest (rest form)))
	     (eq (first form) 'quote))
	(let* ((constant (second form))
	       (immediate (convert-constant-to-immediate constant system)))
	  (if (null immediate)
	      (replace-load-time-value-with-lexical initial-instruction input)
	      (replace-load-time-value-with-immediate immediate input)))
	(replace-load-time-value-with-lexical initial-instruction input))))

;;; Main entry point.
(defun eliminate-load-time-value-inputs (initial-instruction system)
  (assert (typep initial-instruction 'cleavir-ir:top-level-enter-instruction))
  (cleavir-ir:map-instructions-arbitrary-order
   (lambda (instruction)
     (loop for input in (cleavir-ir:inputs instruction)
	   when (typep input 'cleavir-ir:load-time-value-input)
	     do (eliminate-load-time-value-input
		 initial-instruction input system)))
   initial-instruction))
