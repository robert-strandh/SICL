(cl:in-package #:cleavir-mir-interpreter)

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
  (if (typep datum 'cleavir-mir:constant-input)
      `(quote ,(cleavir-mir:value datum))
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
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on TRANSLATE-SIMPLE-INSTRUCTION.

(defmethod translate-simple-instruction
    ((instruction cleavir-mir:enclose-instruction) inputs outputs)
  (declare (ignore inputs))
  `(setq ,(first outputs)
	 ,(layout-procedure (cleavir-mir:code instruction))))

(defmethod translate-simple-instruction
    ((instruction cleavir-mir:assignment-instruction) inputs outputs)
  `(setq ,(first outputs) ,(first inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-mir:return-instruction) inputs outputs)
  `(return (values ,@inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-mir:funcall-instruction) inputs outputs)
  (let ((temps (loop for output in outputs collect (gensym))))
    `(multiple-value-bind ,temps (funcall ,(first inputs) ,@(rest inputs))
       (setq ,@(loop for out in outputs
		     for temp in temps
		     collect out
		     collect temp)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-mir:tailcall-instruction) inputs outputs)
  (declare (ignore outputs))
  `(return (funcall ,(first inputs) ,@(rest inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-mir:the-instruction) inputs outputs)
  (declare (ignore outputs))
  `(unless (typep ,(first inputs) ',(cleavir-mir:value-type instruction))
     (error 'type-error
	    :expected-type ',(cleavir-mir:value-type instruction)
	    :datum ,(first inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-mir:fdefinition-instruction) inputs outputs)
  `(setq ,(first outputs)
	 (fdefinition ,(first inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-mir:car-instruction) inputs outputs)
  `(setq ,(first outputs)
	 (car ,(first inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-mir:cdr-instruction) inputs outputs)
  `(setq ,(first outputs)
	 (cdr ,(first inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-mir:rplaca-instruction) inputs outputs)
  (declare (ignore outputs))
  `(rplaca ,(first inputs) ,(second inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-mir:rplacd-instruction) inputs outputs)
  (declare (ignore outputs))
  `(rplacd ,(first inputs) ,(second inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-mir:t-aref-instruction) inputs outputs)
  `(setq ,(first outputs)
	 (row-major-aref ,(first inputs) ,(second inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-mir:bit-aref-instruction) inputs outputs)
  `(setq ,(first outputs)
	 (row-major-aref ,(first inputs) ,(second inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-mir:unsigned-byte-8-aref-instruction) inputs outputs)
  `(setq ,(first outputs)
	 (row-major-aref ,(first inputs) ,(second inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-mir:short-float-aref-instruction) inputs outputs)
  `(setq ,(first outputs)
	 (row-major-aref ,(first inputs) ,(second inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-mir:single-float-aref-instruction) inputs outputs)
  `(setq ,(first outputs)
	 (row-major-aref ,(first inputs) ,(second inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-mir:double-float-aref-instruction) inputs outputs)
  `(setq ,(first outputs)
	 (row-major-aref ,(first inputs) ,(second inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-mir:long-float-aref-instruction) inputs outputs)
  `(setq ,(first outputs)
	 (row-major-aref ,(first inputs) ,(second inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-mir:t-aset-instruction) inputs outputs)
  (declare (ignore outputs))
  `(setf (row-major-aref ,(first inputs) ,(second inputs))
	 ,(third inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-mir:bit-aset-instruction) inputs outputs)
  (declare (ignore outputs))
  `(setf (row-major-aref ,(first inputs) ,(second inputs))
	 ,(third inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-mir:unsigned-byte-8-aset-instruction) inputs outputs)
  (declare (ignore outputs))
  `(setf (row-major-aref ,(first inputs) ,(second inputs))
	 ,(third inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-mir:short-float-aset-instruction) inputs outputs)
  (declare (ignore outputs))
  `(setf (row-major-aref ,(first inputs) ,(second inputs))
	 ,(third inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-mir:single-float-aset-instruction) inputs outputs)
  (declare (ignore outputs))
  `(setf (row-major-aref ,(first inputs) ,(second inputs))
	 ,(third inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-mir:double-float-aset-instruction) inputs outputs)
  (declare (ignore outputs))
  `(setf (row-major-aref ,(first inputs) ,(second inputs))
	 ,(third inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-mir:long-float-aset-instruction) inputs outputs)
  (declare (ignore outputs))
  `(setf (row-major-aref ,(first inputs) ,(second inputs))
	 ,(third inputs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on TRANSLATE-BRANCH-INSTRUCTION.

(defmethod translate-branch-instruction
    ((instruction cleavir-mir:eq-instruction) inputs outputs successors)
  `(if (eq ,(first inputs) ,(second inputs))
       (go ,(gethash (first successors) *tags*))
       (go ,(gethash (second successors) *tags*))))

(defmethod translate-branch-instruction
    ((instruction cleavir-mir:typeq-instruction) inputs outputs successors)
  `(if (typep ,(first inputs) ',(cleavir-mir:value-type instruction))
       (go ,(gethash (first successors) *tags*))
       (go ,(gethash (second successors) *tags*))))

(defmethod translate-branch-instruction
    ((instruction cleavir-mir:fixnum-add-instruction) inputs outputs successors)
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
    ((instruction cleavir-mir:fixnum-sub-instruction) inputs outputs successors)
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
    ((instruction cleavir-mir:fixnum-less-instruction) inputs outputs successors)
  (declare (ignore outputs))
  `(if (< ,(first inputs) ,(second inputs))
       (go ,(gethash (first successors) *tags*))
       (go ,(gethash (second successors) *tags*))))

(defmethod translate-branch-instruction
    ((instruction cleavir-mir:fixnum-not-greater-instruction) inputs outputs successors)
  (declare (ignore outputs))
  `(if (<= ,(first inputs) ,(second inputs))
       (go ,(gethash (first successors) *tags*))
       (go ,(gethash (second successors) *tags*))))

(defmethod translate-branch-instruction
    ((instruction cleavir-mir:fixnum-equal-instruction) inputs outputs successors)
  (declare (ignore outputs))
  `(if (= ,(first inputs) ,(second inputs))
       (go ,(gethash (first successors) *tags*))
       (go ,(gethash (second successors) *tags*))))
