(cl:in-package #:sicl-compiler)

(defvar *worklist*)

(defvar *type-info*)

(defun initialize-type-info (initial-instruction)
  (let ((table (make-hash-table :test #'eq)))
    (labels ((traverse (instruction)
	       (unless (gethash instruction table)
		 (setf (gethash instruction table) t)
		 (loop for succ in (cleavir-ir:successors instruction)
		       do (setf (gethash (list instruction succ) *type-info*)
				(sicl-compiler-types:make-t-type-map)))
		 (loop for succ in (cleavir-ir:successors instruction)
		       do (traverse succ)))))
      (traverse initial-instruction))))
			  
(defun initialize-worklist (initial-instruction)
  (let ((table (make-hash-table :test #'eq)))
    (labels ((traverse (instruction)
	       (unless (gethash instruction table)
		 (setf (gethash instruction table) t)
		 (push instruction *worklist*)
		 (loop for succ in (cleavir-ir:successors instruction)
		       do (traverse succ)))))
      (traverse initial-instruction))))

(defgeneric process-instruction (instruction))

(defmethod process-instruction :around (instruction)
  (let ((old-maps (loop for succ in (cleavir-ir:successors instruction)
			collect (gethash (list instruction succ) *type-info*))))
    (call-next-method)
    (loop for succ in (cleavir-ir:successors instruction)
	  for old-map in old-maps
	  do (unless (sicl-compiler-types:type-map-equal
		      old-map (gethash (list instruction succ) *type-info*))
	       (push succ *worklist*)))))

(defun type-inference (initial-instruction)
  (let ((*type-info* (make-hash-table :test #'equal))
	(*worklist* '()))
    (initialize-type-info initial-instruction)
    (initialize-worklist initial-instruction)
    (loop until (null *worklist*)
	  do (process-instruction (pop *worklist*)))
    *type-info*))
    
(defun combine-predecessor-type-maps (instruction)
  (let ((predecessors (cleavir-ir:predecessors instruction)))
    (cond ((null predecessors)
	   (sicl-compiler-types:make-t-type-map))
	  ((null (cdr predecessors))
	   (sicl-compiler-types:copy-type-map
	    (gethash (list (car predecessors) instruction) *type-info*)))
	  (t
	   (reduce #'sicl-compiler-types:type-map-or 
		   (loop for pred in predecessors
			 collect (gethash (list pred instruction)
					  *type-info*)))))))

(defun set-type-map (instruction successor type-map)
  (setf (gethash (list instruction successor) *type-info*) type-map))

;;; By default, we take the OR of the type maps of the incoming arcs,
;;; and propagate the result to every outgoing arc.
(defmethod process-instruction (instruction)
  (let ((result (combine-predecessor-type-maps instruction)))
    (loop for succ in (cleavir-ir:successors instruction)
	  do (setf (gethash (list instruction succ) *type-info*) result))))

(defmethod process-instruction ((instruction cleavir-ir:enter-instruction))
  (let ((result (combine-predecessor-type-maps instruction)))
    (loop for succ in (cleavir-ir:successors instruction)
	  do (set-type-map instruction succ result))))

(defmethod process-instruction ((instruction cleavir-ir:get-argcount-instruction))
  (let ((result (combine-predecessor-type-maps instruction))
	(output (car (cleavir-ir:outputs instruction))))
    (setf (sicl-compiler-types:type-descriptor output result)
	  (sicl-compiler-types:type-descriptor-from-type 'fixnum))
    (set-type-map instruction (car (cleavir-ir:successors instruction)) result)))

(defmethod process-instruction ((instruction cleavir-ir:typeq-instruction))
  (let ((result (combine-predecessor-type-maps instruction)))
    (destructuring-bind (false true) (cleavir-ir:successors instruction)
      (multiple-value-bind (result1 result2)
	  (sicl-compiler-types:split-type-map
	   result
	   (car (cleavir-ir:inputs instruction))
	   (cleavir-ir:value-type instruction))
	(set-type-map instruction false result1)
	(set-type-map instruction true result2)))))

(defun type-descriptor-of-input (type-map input)
  (etypecase input
    (cleavir-ir:lexical-location
     (sicl-compiler-types:type-descriptor input type-map))
    (cleavir-ir:immediate-input
     (let ((value (cleavir-ir:value input)))
       (sicl-compiler-types:type-descriptor-from-type
	`(integer ,value ,value))))
    (cleavir-ir:constant-input
     (let ((value (cleavir-ir:value input)))
       (sicl-compiler-types:type-descriptor-from-type
	(type-of value))))
    (cleavir-ir:global-input
     (sicl-compiler-types:make-t-type-descriptor))))

(defmethod process-instruction ((instruction cleavir-ir:assignment-instruction))
  (let ((result (combine-predecessor-type-maps instruction))
	(input (car (cleavir-ir:inputs instruction)))
	(output (car (cleavir-ir:outputs instruction)))
	(successor (car (cleavir-ir:successors instruction))))
    (setf (sicl-compiler-types:type-descriptor output result)
	  (type-descriptor-of-input result input))
    (set-type-map instruction successor result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Trim the instruction graph by replacing TYPEQ instructions that
;;; have one "impossible" successor.  A successor is impossible if the
;;; transition from the TYPEQ instruction to that successor contains a
;;; variable with NIL type. 

(defun trim-instruction-graph (initial-instruction type-info)
  (let ((table (make-hash-table :test #'eq)))
    (labels ((traverse (instruction)
	       (unless (gethash instruction table)
		 (setf (gethash instruction table) t)
		 (when (typep instruction 'cleavir-ir:typeq-instruction)
		   (destructuring-bind (false true)
		       (cleavir-ir:successors instruction)
		     (cond ((sicl-compiler-types:impossible-type-map-p
			     (gethash (list instruction false) type-info))
			    (setf (cleavir-ir:successors instruction)
				  (list true))
			    (setf (cleavir-ir:inputs instruction) '())
			    (change-class instruction
					  'cleavir-ir:nop-instruction))
			   ((sicl-compiler-types:impossible-type-map-p
			     (gethash (list instruction true) type-info))
			    (setf (cleavir-ir:successors instruction)
				  (list false))
			    (setf (cleavir-ir:inputs instruction) '())
			    (change-class instruction
					  'cleavir-ir:nop-instruction))
			   (t nil))))
		 (mapc #'traverse (cleavir-ir:successors instruction)))))
      (traverse initial-instruction)
      ;; With such radical modifications to the instruction graph
      ;; (entire subgraphs may have been removed), we must
      ;; reinitialize the set of defining/using instructions of each
      ;; datum.
      (cleavir-ir:reinitialize-data initial-instruction))))
