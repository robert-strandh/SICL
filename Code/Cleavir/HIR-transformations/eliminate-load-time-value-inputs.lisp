(cl:in-package #:cleavir-hir-transformations)

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

(defun eliminate-load-time-value-inputs (initial-instruction system)
  (assert (typep initial-instruction 'cleavir-ir:top-level-enter-instruction))
  (cleavir-ir:map-instructions-arbitrary-order
   (lambda (instruction)
     (loop for input in (cleavir-ir:inputs instruction)
	   when (typep input 'cleavir-ir:load-time-value-input)
	     do (eliminate-load-time-value-input
		 initial-instruction input system)))
   initial-instruction))
