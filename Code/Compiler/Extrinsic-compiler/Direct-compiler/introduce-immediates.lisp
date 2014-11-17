(cl:in-package #:sicl-direct-extrinsic-compiler)

(defun maybe-replace (input)
  (if (typep input 'cleavir-ir:constant-input)
      (let ((value (cleavir-ir:value input)))
	(cond ((and (integerp value)
		    (<= #.(- (expt 2 31))
			value
			#.(1- (expt 2 31))))
	       (make-instance 'cleavir-ir:immediate-input
		 :value (ash value 1)))
	      ((characterp value)
	       (make-instance 'cleavir-ir:immediate-input
		 :value (+ (ash (char-code value) 3) 3)))
	      (t
	       input)))
      input))

;;; FIXME: this function should call a processor-specific function to
;;; determine whether a constant can be converted to an immediate.
(defun introduce-immediates (initial-instruction)
  (let ((table (make-hash-table :test #'eq)))
    (flet ((handle-inputs (instruction)
	     (setf (cleavir-ir:inputs instruction)
		   (mapcar #'maybe-replace (cleavir-ir:inputs instruction)))))
      (labels ((traverse (instruction)
		 (unless (gethash instruction table)
		   (setf (gethash instruction table) t)
		   (handle-inputs instruction)
		   (mapc #'traverse (cleavir-ir:successors instruction)))))
	(traverse initial-instruction)))))

  
