(cl:in-package #:sicl-extrinsic-file-compiler)

(defmethod cleavir-hir-transformations:introduce-immediate
    (constant
     (implementation sicl-target-sicl:sicl)
     (processor cleavir-processor-x86-64:x86-64)
     os)
  (let ((value (cleavir-ir:value constant)))
    (cond ((and (typep value 'integer)
		(<= #.(- (expt 2 30)) value #.(1- (expt 2 30))))
	   (cleavir-ir:make-immediate-input (* 2 value)))
	  ((typep value 'character)
	   ;; FIXME: Currently, we depend on the host having the same
	   ;; character encoding as the target.
	   (cleavir-ir:make-immediate-input (+ #b11 (* 4 (char-code value)))))
	  (t
	   constant))))

   
