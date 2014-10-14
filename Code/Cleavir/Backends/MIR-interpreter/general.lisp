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
