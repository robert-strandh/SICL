(cl:in-package #:cleavir-mir-interpreter)

(defgeneric read-value (input environment))

(defmethod read-value ((input cleavir-mir:lexical-location) environment)
  (load-lexical input environment))

(defmethod read-value ((input cleavir-mir:constant-input) environment)
  (declare (ignore environment))
  (cleavir-mir:value input))

(defgeneric write-value (output environment value))

(defmethod write-value ((output cleavir-mir:lexical-location) environment value)
  (store-lexical output environment value))

(defgeneric execute-instruction (instruction environment))

(defparameter *step* nil)

(defmethod execute-instruction :around (instruction environment)
  (declare (ignore instruction environment))
  (when *step* (break))
  (call-next-method))

(defun interpret-mir (initial-instruction)
  (let ((environment (list (make-hash-table :test #'eq))))
    (let ((next initial-instruction))
      (catch 'return
	(loop do (setf next (execute-instruction next environment)))))))

;; (defun interpret-mir (enter-instruction environment arguments)
;;   (push (make-hash-table :test #'eq) environment)
;;   (let* ((lambda-list (cleavir-mir:lambda-list enter-instruction)) 
;; 	 (pos (position-if (lambda (x) (member x '(&optional &key)))
;; 			   lambda-list)))
;;     (cond ((null pos)
;; 	   ;; Only required arguments.
;; 	   (when (< (length arguments) (length lambda-list))
;; 	     (error "too few arguments"))
;; 	   (when (> (length arguments) (length lambda-list))
;; 	     (error "too many arguments"))
;; 	   (loop for value in arguments
;; 		 for variable in lambda-list
;; 		 do (store-lexical variable environment value)))
;; 	  (t
;; 	   (error "can't handle &optional or &key yet")))
;;     (let ((next (first (cleavir-mir:successors enter-instruction))))
;;       (catch 'return
;; 	(loop do (setf next (execute-instruction next environment)))))))

;; (defun enclose (enter-instruction environment)
;;   (lambda (&rest arguments)
;;     (interpret-mir enter-instruction environment arguments)))

(defmethod execute-instruction
    ((instruction cleavir-mir:assignment-instruction) environment)
  (let* ((input (car (cleavir-mir:inputs instruction)))
	 (value (read-value input environment))
	 (output (car (cleavir-mir:outputs instruction))))
    (write-value output environment value)
    (first (cleavir-mir:successors instruction))))

(defmethod execute-instruction
    ((instruction cleavir-mir:typeq-instruction) environment)
  (let* ((inputs (cleavir-mir:inputs instruction))
	 (datum (first inputs))
	 (type (second inputs))
	 (successors (cleavir-mir:successors instruction)))
    (if (typep (read-value datum environment) (read-value type environment))
	(first successors)
	(second successors))))
	       
(defmethod execute-instruction
    ((instruction cleavir-mir:return-instruction) environment)
  (let ((values (loop for input in (cleavir-mir:inputs instruction)
		      collect (read-value input environment))))
    (throw 'return (apply #'values values))))

(defmethod execute-instruction
    ((instruction cleavir-mir:funcall-instruction) environment)
  (let* ((inputs (cleavir-mir:inputs instruction))
	 (outputs (cleavir-mir:outputs instruction))
	 (function-datum (first inputs))
	 (function (read-value function-datum environment))
	 (arguments (loop for datum in (rest inputs)
			  collect (read-value datum environment))))
    ;; First assign NIL to all outputs in case there is an
    ;; insufficient number of values.
    (loop for output in outputs
	  do (write-value output environment nil))
    ;; Then assign the values returned by the call.
    (loop for output in outputs
	  for value in (multiple-value-list (apply function arguments))
	  do (write-value output environment value))
    (first (cleavir-mir:successors instruction))))

(defmethod execute-instruction
    ((instruction cleavir-mir:fdefinition-instruction) environment)
  (let* ((inputs (cleavir-mir:inputs instruction))
	 (outputs (cleavir-mir:outputs instruction))
	 (function-name (read-value (first inputs) environment))
	 (function (fdefinition function-name)))
    (write-value (first outputs) environment function)
    (first (cleavir-mir:successors instruction))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Execute TAILCALL-INSTRUCTION.
;;;
;;; We execute this instruction by throwing the values resulting from
;;; the function call to the RETURN catch tag. 

(defmethod execute-instruction
    ((instruction cleavir-mir:tailcall-instruction) environment)
  (let* ((inputs (cleavir-mir:inputs instruction))
	 (function-datum (first inputs))
	 (function (read-value function-datum environment))
	 (arguments (loop for datum in (rest inputs)
			  collect (read-value datum environment))))
    (throw 'return (apply function arguments))))	 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Execute EQ-INSTRUCTION.
;;;
;;; Recall that the EQ-INSTRUCTION is an instruction with two inputs,
;;; no outputs, and two successors.  The two inputs are the objects to
;;; be compared using EQ.  The first output is chosen if the
;;; comparison yields true, and the second output is chosen if the
;;; comparison yields false.

(defmethod execute-instruction
    ((instruction cleavir-mir:eq-instruction) environment)
  (destructuring-bind (x y) (cleavir-mir:inputs instruction)
    (let ((xx (read-value x environment))
	  (yy (read-value y environment)))
      (destructuring-bind (true false) (cleavir-mir:successors instruction)
	(if (eq xx yy) true false)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Execute ENCLOSE-INSTRUCTION.

;;; FIXME: this code is identical to that of the AST interpreter.  Try
;;; to factor it.
(defun load-environment (environment arguments lambda-list)
  (let ((table (make-hash-table :test #'eq))
	(remaining arguments)
	(ll lambda-list))
    (tagbody
     required
       (cond ((null remaining)
	      ;; We ran out of arguments. 
	      (cond ((null ll)
		     ;; There are no more parameters in the lambda
		     ;; list.  We are done.
		     (go end))
		    ((eq (first ll) '&optional)
		     ;; There might be &optional parameters and maybe
		     ;; also &key parameters.  The corresponding
		     ;; SUPPLIED-P parameters must then be initialized
		     ;; to NIL.
		     (pop ll)
		     (go no-optional))
		    ((eq (first ll) '&key)
		     ;; There might be &key parameters.  The
		     ;; corresponding SUPPLIED-P parameters must then
		     ;; be initialized to NIL.
		     (pop ll)
		     (go no-key))
		    (t
		     ;; There are more required parameters in the
		     ;; lambda list, even though there are no more
		     ;; arguments.
		     (error "Too few arguments"))))
	     ((null ll)
	      ;; There are more arguments, but the remaining lambda
	      ;; list is empty, so there are no parameters to match
	      ;; those arguments to.  
	      (error "Too many arguments"))
	     ((eq (first ll) '&optional)
	      ;; There are more arguments, the lambda list has more
	      ;; entries in it, and the first entry in the lambda list
	      ;; is the lambda-list keyword &OPTIONAL.
	      (pop ll)
	      (go optional))
	     ((eq (first ll) '&key)
	      ;; There are more arguments, the lambda list has more
	      ;; entries in it, and the first entry in the lambda list
	      ;; is the lambda-list keyword &KEY.
	      (pop ll)
	      (go key))
	     (t
	      ;; There are more arguments, the lambda list has more
	      ;; entries in it, and the first entry in the lambda list
	      ;; is a required parameter in the form of a lexical
	      ;; variable.  Add the first of the remaining arguments
	      ;; to the hash table representing the environment.
	      (setf (gethash (pop ll) table) (pop remaining))
	      (go required)))
     optional
       (cond ((null remaining)
	      ;; We ran out of arguments.  But there might be more
	      ;; &optional and perhaps also &key parameters left in
	      ;; the lambda list.  We must initialize the
	      ;; corresponding SUPPLIED-P parameters of those to NIL.
	      (go no-optional))
	     ((null ll)
	      ;; There are more arguments, but the remaining lambda
	      ;; list is empty, so there are no parameters to match
	      ;; those arguments to.  
	      (error "Too many arguments"))
	     ((eq (first ll) '&key)
	      ;; There are more arguments, the lambda list has more
	      ;; entries in it, and the first entry in the lambda list
	      ;; is the lambda-list keyword &KEY.
	      (pop ll)
	      (go key))
	     (t
	      ;; There are more arguments, the lambda list has more
	      ;; entries in it, and the first entry in the lambda list
	      ;; is an optional parameter in the form of a list of two
	      ;; lexical variables, one for the argument itself, and
	      ;; one indicating that the argument has been supplied.
	      ;; Add the first of the remaining arguments to the hash
	      ;; table representing the environment.
	      (setf (gethash (first (first ll)) table) (pop remaining))
	      (setf (gethash (second (pop ll)) table) t)
	      (go optional)))
     key
       (let ((default (list nil))) ; for use with GETF
	 (cond ((null remaining)
		;; We ran out of arguments.  But there might be more
		;; &key parameters left in the lambda list.  We must
		;; initialize the corresponding SUPPLIED-P parameters
		;; of those to NIL.
		(go no-key))
	       ((null ll)
		;; There are more arguments, but the remaining lambda
		;; list is empty, so there are no parameters to match
		;; those arguments to.
		(if (getf remaining :allow-other-keys)
		    ;; The caller disabled mandatory keyword matching.
		    (go end)
		    (error "Too many arguments")))
	       ((eq (first ll) '&allow-other-keys)
		(go end))
	       (t
		;; There are more arguments, the lambda list has more
		;; entries in it, and the first entry in the lambda
		;; list is a key parameter in the form of a list of a
		;; keyword and two lexical variables, one for the
		;; argument itself, and one indicating that the
		;; argument has been supplied.
		(let* ((keyword (first (first ll)))
		       ;; See if there is an argument for this parameter. 
		       (arg (getf remaining keyword default)))
		  (if (eq arg default)
		      ;; No argument was supplied for this keyword parameter
		      (progn (setf (gethash (third (pop ll)) table) nil)
			     (go key))
		      ;; We found an argument for this keyword parameter.
		      (progn 
			;; Enter the argument into the environment
			(setf (gethash (second (first ll)) table)
			      arg)
			;; Set the supplied-p parameter to T.
			(setf (gethash (third (pop ll)) table) t)
			;; Remove any keyword arguments with the same
			;; key from the remaining argument list.
			(loop while (remf remaining keyword))
			(go key)))))))
     no-optional
       ;; We come here when we have run out of arguments, but we might
       ;; have more &optional parameters, and perhaps also &key
       ;; parameters.  We must initialize the corresponding SUPPLIED-P
       ;; parameters to NIL.
       (cond ((null ll)
	      ;; We ran out of parameters.
	      (go end))
	     ((eq (first ll) '&key)
	      ;; We ran out of &optional parameters, but there might
	      ;; still be &key parameters left.
	      (pop ll)
	      (go no-key))
	     (t
	      ;; We have at last one &optional parameter.  We must
	      ;; initialize the corresponding SUPPLIED-P parameter to
	      ;; NIL.
	      (setf (gethash (second (pop ll)) table) nil)
	      ;; There might be more &optional parameters. 
	      (go no-optional)))
     no-key
       ;; We come here when we have run out of arguments, but we might
       ;; have more &key parameters.  We must initialize the
       ;; corresponding SUPPLIED-P parameters to NIL.
       (cond ((or (null ll) (eq (car ll) '&allow-other-keys))
	      ;; We ran out of parameters.
	      (go end))
	     (t
	      ;; We have at least one &key parameter.  We must
	      ;; initialize the corresponding SUPPLIED-P parameter to
	      ;; NIL.
	      (setf (gethash (third (pop ll)) table) nil)
	      ;; There might be more &key parameters.
	      (go no-key)))
     end)
    (cons table environment)))

(defmethod execute-instruction
    ((instruction cleavir-mir:enclose-instruction) environment)
  (let ((fun
	  (lambda (&rest arguments)
	    (let* ((enter-instruction (cleavir-mir:code instruction))
		   (lambda-list (cleavir-mir:lambda-list enter-instruction))
		   (new-env (load-environment environment arguments lambda-list)))
	      (let ((next (first (cleavir-mir:successors enter-instruction))))
		(catch 'return 
		  (loop do (setf next (execute-instruction next new-env)))))))))
    (write-value (first (cleavir-mir:outputs instruction)) environment fun))
  (first (cleavir-mir:successors instruction)))
