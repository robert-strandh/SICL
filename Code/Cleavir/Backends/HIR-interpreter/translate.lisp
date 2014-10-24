(cl:in-package #:cleavir-hir-interpreter)

;;; The first argument to this function is an instruction that has a
;;; single successor.  Whether a GO is required at the end of this
;;; function is determined by the code layout algorithm.  
;;; 
;;; The inputs are forms to be evaluated.  The outputs are symbols
;;; that are names of variables.
(defgeneric translate-simple-instruction (instruction inputs outputs))

(defgeneric translate-branch-instruction (instruction inputs outputs successors))

(defvar *ownerships*)
(defvar *tags*)
(defvar *vars*)

(defun translate-datum (datum)
  (if (typep datum 'cleavir-ir:constant-input)
      `(quote ,(cleavir-ir:value datum))
      (let ((var (gethash datum *vars*)))
	(when (null var)
	  (setf var (gensym))
	  (setf (gethash datum *vars*) var))
	var)))

(defun translate-lambda-list-item (item)
  (cond ((symbolp item)
	 item)
	((consp item)
	 (ecase (length item)
	   (2 (list (translate-datum (first item))
		    nil
		    (translate-datum (second item))))
	   (3 (list (list (first item)
			  (translate-datum (second item)))
		    nil
		    (translate-datum (third item))))))
	(t
	 (translate-datum item))))

(defun translate-lambda-list (lambda-list)
  (mapcar #'translate-lambda-list-item lambda-list))

(defun layout-procedure (initial-instruction)
  (labels ((layout (instruction)
	     (let ((tag (gethash instruction *tags*)))
	       (when (null tag)
		 (setf (gethash instruction *tags*) (gensym))
		 (let ((inputs (loop for i in (cleavir-ir:inputs instruction)
				     collect (translate-datum i)))
		       (outputs (loop for o in (cleavir-ir:outputs instruction)
				      collect (translate-datum o))))
		   (ecase (length (cleavir-ir:successors instruction))
		     (0
		      (list (translate-simple-instruction
			     instruction inputs outputs)))
		     (1
		      (let* ((succ (first (cleavir-ir:successors instruction)))
			     (preds (cleavir-ir:predecessors succ)))
			(if (eq (gethash instruction *ownerships*)
				(gethash succ *ownerships*))
			    (if (gethash succ *tags*)
				(list* (translate-simple-instruction
					instruction inputs outputs)
				       `(go ,(gethash succ *tags*))
				       (layout succ))
				(if (> (length preds) 1)
				    (list* (translate-simple-instruction
					    instruction inputs outputs)
					   (gethash succ *tags*)
					   (layout succ))
				    (cons (translate-simple-instruction
					   instruction inputs outputs)
					  (layout succ))))
			    (list* (translate-simple-instruction
				    instruction inputs outputs)
				   `(go (gethash succ *tags*))
				   (layout succ)))))
		     (2
		      (let ((successors (cleavir-ir:successors instruction)))
			(destructuring-bind (true false) successors
			  (let ((tt (layout true))
				(ff (layout false)))
			    (cons (translate-branch-instruction
				   instruction inputs outputs successors)
				  (append (list (gethash true *tags*))
					  tt
					  (list (gethash false *tags*))
					  ff))))))))))))
    `(lambda ,(translate-lambda-list
	       (cleavir-ir:lambda-list initial-instruction))
       (block nil
	 (let ,(loop for var being each hash-key of *ownerships*
		     using (hash-value owner)
		     when (and (typep var 'cleavir-ir:lexical-location)
			       (eq owner initial-instruction)
			       (not (member var (cleavir-ir:outputs
						 initial-instruction))))
		       collect (translate-datum var))
	   (tagbody 
	      ,@(layout (first (cleavir-ir:successors initial-instruction)))))))))

(defun translate (initial-instruction)
  (let* ((enter-inst
	   (cleavir-ir:make-enter-instruction '() initial-instruction))
	 (*ownerships* (cleavir-lexical-depth:compute-ownerships enter-inst))
	 (*tags* (make-hash-table :test #'eq))
	 (*vars* (make-hash-table :test #'eq)))
    (layout-procedure enter-inst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on TRANSLATE-SIMPLE-INSTRUCTION.

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:enclose-instruction) inputs outputs)
  (declare (ignore inputs))
  `(setq ,(first outputs)
	 ,(layout-procedure (cleavir-ir:code instruction))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:assignment-instruction) inputs outputs)
  `(setq ,(first outputs) ,(first inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:return-instruction) inputs outputs)
  `(return (values ,@inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:funcall-instruction) inputs outputs)
  (let ((temps (loop for output in outputs collect (gensym))))
    `(multiple-value-bind ,temps (funcall ,(first inputs) ,@(rest inputs))
       (setq ,@(loop for out in outputs
		     for temp in temps
		     collect out
		     collect temp)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:tailcall-instruction) inputs outputs)
  (declare (ignore outputs))
  `(return (funcall ,(first inputs) ,@(rest inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:the-instruction) inputs outputs)
  (declare (ignore outputs))
  `(unless (typep ,(first inputs) ',(cleavir-ir:value-type instruction))
     (error 'type-error
	    :expected-type ',(cleavir-ir:value-type instruction)
	    :datum ,(first inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:fdefinition-instruction) inputs outputs)
  `(setq ,(first outputs)
	 (fdefinition ,(first inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:car-instruction) inputs outputs)
  `(setq ,(first outputs)
	 (car ,(first inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:cdr-instruction) inputs outputs)
  `(setq ,(first outputs)
	 (cdr ,(first inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:rplaca-instruction) inputs outputs)
  (declare (ignore outputs))
  `(rplaca ,(first inputs) ,(second inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:rplacd-instruction) inputs outputs)
  (declare (ignore outputs))
  `(rplacd ,(first inputs) ,(second inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:t-aref-instruction) inputs outputs)
  `(setq ,(first outputs)
	 (row-major-aref ,(first inputs) ,(second inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:bit-aref-instruction) inputs outputs)
  `(setq ,(first outputs)
	 (row-major-aref ,(first inputs) ,(second inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:unsigned-byte-8-aref-instruction) inputs outputs)
  `(setq ,(first outputs)
	 (row-major-aref ,(first inputs) ,(second inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:short-float-aref-instruction) inputs outputs)
  `(setq ,(first outputs)
	 (row-major-aref ,(first inputs) ,(second inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:single-float-aref-instruction) inputs outputs)
  `(setq ,(first outputs)
	 (row-major-aref ,(first inputs) ,(second inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:double-float-aref-instruction) inputs outputs)
  `(setq ,(first outputs)
	 (row-major-aref ,(first inputs) ,(second inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:long-float-aref-instruction) inputs outputs)
  `(setq ,(first outputs)
	 (row-major-aref ,(first inputs) ,(second inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:t-aset-instruction) inputs outputs)
  (declare (ignore outputs))
  `(setf (row-major-aref ,(first inputs) ,(second inputs))
	 ,(third inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:bit-aset-instruction) inputs outputs)
  (declare (ignore outputs))
  `(setf (row-major-aref ,(first inputs) ,(second inputs))
	 ,(third inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:unsigned-byte-8-aset-instruction) inputs outputs)
  (declare (ignore outputs))
  `(setf (row-major-aref ,(first inputs) ,(second inputs))
	 ,(third inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:short-float-aset-instruction) inputs outputs)
  (declare (ignore outputs))
  `(setf (row-major-aref ,(first inputs) ,(second inputs))
	 ,(third inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:single-float-aset-instruction) inputs outputs)
  (declare (ignore outputs))
  `(setf (row-major-aref ,(first inputs) ,(second inputs))
	 ,(third inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:double-float-aset-instruction) inputs outputs)
  (declare (ignore outputs))
  `(setf (row-major-aref ,(first inputs) ,(second inputs))
	 ,(third inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:long-float-aset-instruction) inputs outputs)
  (declare (ignore outputs))
  `(setf (row-major-aref ,(first inputs) ,(second inputs))
	 ,(third inputs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on TRANSLATE-BRANCH-INSTRUCTION.

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:eq-instruction) inputs outputs successors)
  `(if (eq ,(first inputs) ,(second inputs))
       (go ,(gethash (first successors) *tags*))
       (go ,(gethash (second successors) *tags*))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:typeq-instruction) inputs outputs successors)
  `(if (typep ,(first inputs) ',(cleavir-ir:value-type instruction))
       (go ,(gethash (first successors) *tags*))
       (go ,(gethash (second successors) *tags*))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnum-add-instruction) inputs outputs successors)
  (let ((result (gensym)))
    `(let ((,result (+ ,(first inputs) ,(second inputs))))
       (cond ((typep result 'fixnum)
	      (setq ,(first outputs) ,result)
	      (go ,(gethash (first successors) *tags*)))
	     ((plusp ,result)
	      (setq ,(first outputs)
		    (+ ,result (* 2 most-negative-fixnum)))
	      (go ,(gethash (second successors) *tags*)))
	     (t
	      (setq ,(first outputs)
		    (- ,result (* 2 most-negative-fixnum)))
	      (go ,(gethash (second successors) *tags*)))))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnum-sub-instruction) inputs outputs successors)
  (let ((result (gensym)))
    `(let ((,result (- ,(first inputs) ,(second inputs))))
       (cond ((typep result 'fixnum)
	      (setq ,(first outputs) ,result)
	      (go ,(gethash (first successors) *tags*)))
	     ((plusp ,result)
	      (setq ,(first outputs)
		    (+ ,result (* 2 most-negative-fixnum)))
	      (go ,(gethash (second successors) *tags*)))
	     (t
	      (setq ,(first outputs)
		    (- ,result (* 2 most-negative-fixnum)))
	      (go ,(gethash (second successors) *tags*)))))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnum-less-instruction) inputs outputs successors)
  (declare (ignore outputs))
  `(if (< ,(first inputs) ,(second inputs))
       (go ,(gethash (first successors) *tags*))
       (go ,(gethash (second successors) *tags*))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnum-not-greater-instruction) inputs outputs successors)
  (declare (ignore outputs))
  `(if (<= ,(first inputs) ,(second inputs))
       (go ,(gethash (first successors) *tags*))
       (go ,(gethash (second successors) *tags*))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnum-equal-instruction) inputs outputs successors)
  (declare (ignore outputs))
  `(if (= ,(first inputs) ,(second inputs))
       (go ,(gethash (first successors) *tags*))
       (go ,(gethash (second successors) *tags*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Main entry point.

(defun interpret-hir (initial-instruction)
  (funcall (compile nil (translate initial-instruction))))
