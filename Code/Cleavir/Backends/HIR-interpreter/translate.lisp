(cl:in-package #:cleavir-hir-interpreter)

;;; The first argument to this function is an instruction that has a
;;; single successor.  Whether a GO is required at the end of this
;;; function is determined by the code layout algorithm.  
;;; 
;;; The inputs are forms to be evaluated.  The outputs are symbols
;;; that are names of variables.
(defgeneric translate-simple-instruction (instruction inputs outputs))

(defgeneric translate-branch-instruction (instruction inputs outputs successors))

(defvar *basic-blocks*)
(defvar *ownerships*)
(defvar *tags*)
(defvar *vars*)

(defun translate-datum (datum)
  (etypecase datum
    ((or cleavir-ir:constant-input cleavir-ir:immediate-input)
     `(quote ,(cleavir-ir:value datum)))
    (cleavir-ir:load-time-value-input
     `(load-time-value ,(cleavir-ir:form datum)
                       ,(cleavir-ir:read-only-p datum)))
    (cleavir-ir:lexical-location
     (let ((var (gethash datum *vars*)))
       (when (null var)
         (setf var (gensym (symbol-name (cleavir-ir:name datum))))
         (setf (gethash datum *vars*) var))
       var))
    (cleavir-ir:values-location
     (let ((var (gethash datum *vars*)))
       (when (null var)
         (setf var (gensym "V"))
         (setf (gethash datum *vars*) var))
       var))))

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

(defun layout-basic-block (basic-block)
  (destructuring-bind (first last owner) basic-block
    (declare (ignore owner))
    (append (loop for instruction = first
		    then (first (cleavir-ir:successors instruction))
		  for inputs = (cleavir-ir:inputs instruction)
		  for input-vars = (mapcar #'translate-datum inputs)
		  for outputs = (cleavir-ir:outputs instruction)
		  for output-vars = (mapcar #'translate-datum outputs)
		  until (eq instruction last)
		  collect (translate-simple-instruction
			   instruction input-vars output-vars))
	    (let* ((inputs (cleavir-ir:inputs last))
		   (input-vars (mapcar #'translate-datum inputs))
		   (outputs (cleavir-ir:outputs last))
		   (output-vars (mapcar #'translate-datum outputs))
		   (successors (cleavir-ir:successors last))
		   (successor-tags (loop for successor in successors
					 collect (gethash successor *tags*))))
	      (if (= (length successors) 1)
		  (list (translate-simple-instruction
			 last input-vars output-vars)
			`(go ,(gethash (first successors) *tags*)))
		  (list (translate-branch-instruction
			 last input-vars output-vars successor-tags)))))))

(defun layout-procedure (initial-instruction)
  (let* ((basic-blocks (remove initial-instruction
			       *basic-blocks*
			       :test-not #'eq :key #'third))
	 (first (find initial-instruction basic-blocks
		      :test #'eq :key #'first))
	 (rest (remove first basic-blocks :test #'eq)))
    ;; Assign tags to all basic block except the first one
    (loop for block in rest
	  for instruction = (first block)
	  do (setf (gethash instruction *tags*) (gensym)))
    `(lambda ,(translate-lambda-list
               (cleavir-ir:lambda-list initial-instruction))
       (prog ,(loop for var being each hash-key of *ownerships*
                      using (hash-value owner)
                    when (and (typep var '(or
                                           cleavir-ir:lexical-location
                                           cleavir-ir:values-location))
                              (eq owner initial-instruction)
                              (not (member var (cleavir-ir:outputs
                                                initial-instruction))))
                      collect (translate-datum var))
          ,@(layout-basic-block first)
          ,@(loop for basic-block in rest
                  collect (gethash (first basic-block) *tags*)
                  append (layout-basic-block basic-block))))))

(defun translate (initial-instruction)
  (let* ((ownerships
          (cleavir-hir-transformations:compute-location-owners
           initial-instruction))
	 (*ownerships* ownerships)
	 (*basic-blocks* (cleavir-basic-blocks:basic-blocks initial-instruction))
	 (*tags* (make-hash-table :test #'eq))
	 (*vars* (make-hash-table :test #'eq)))
    (layout-procedure initial-instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on TRANSLATE-SIMPLE-INSTRUCTION.

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:enclose-instruction) inputs outputs)
  (let ((enter-instruction (cleavir-ir:code instruction)))
    `(setq ,(first outputs)
           ,(if (null inputs) ; skip the closure.
                (layout-procedure enter-instruction)
                `(let ((,(translate-datum
                          (first
                           (cleavir-ir:outputs enter-instruction)))
                         (list ,@inputs)))
                   ,(layout-procedure enter-instruction))))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:enter-instruction) inputs outputs)
  (declare (ignore inputs outputs))
  (gensym))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:assignment-instruction) inputs outputs)
  `(setq ,(first outputs) ,(first inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:funcall-instruction) inputs outputs)
  `(setf ,(first outputs)
	 (multiple-value-list (funcall ,(first inputs) ,@(rest inputs)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:multiple-value-call-instruction) inputs outputs)
  `(setf ,(first outputs)
	 (multiple-value-list
	  (funcall ,(first inputs)
		   (append ,@(rest inputs))))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:tailcall-instruction) inputs outputs)
  (declare (ignore outputs))
  `(return (funcall ,(first inputs) ,@(rest inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:funcall-no-return-instruction) inputs outputs)
  `(funcall ,(first inputs) ,@(rest inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:the-instruction) inputs outputs)
  (declare (ignore outputs))
  (gensym)
  ;; doesn't work ltv inputs and such
  #+(or)`(setq ,(first inputs) (the ,(cleavir-ir:value-type instruction)
                              ,(first inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:the-values-instruction) inputs outputs)
  ;; FIXME: annoying to do.
  (gensym))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:dynamic-allocation-instruction) inputs outputs)
  (declare (ignore inputs outputs))
  (gensym))

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
    ((instruction cleavir-ir:fixed-to-multiple-instruction) inputs outputs)
  `(setq ,(first outputs)
	 (list ,@inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:multiple-to-fixed-instruction) inputs outputs)
  (let ((temp (gensym)))
    `(let ((,temp ,(first inputs)))
       (declare (ignorable ,temp))
       ,@(loop for output in outputs
	       collect `(setf ,output (pop ,temp))))))
  
(defmethod translate-simple-instruction
    ((instruction cleavir-ir:unwind-instruction) inputs outputs)
  (gensym))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:nop-instruction) inputs outputs)
  (declare (ignore inputs outputs))
  (gensym))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:create-cell-instruction) inputs outputs)
  (declare (ignore inputs))
  `(setq ,(first outputs) (cons nil nil)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:read-cell-instruction) inputs outputs)
  `(setq ,(first outputs) (car ,(first inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:write-cell-instruction) inputs outputs)
  (declare (ignore outputs))
  `(rplaca ,(first inputs) ,(second inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:fetch-instruction) inputs outputs)
  `(setq ,(first outputs)
         (nth ,(second inputs) ,(first inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:symbol-value-instruction) inputs outputs)
  `(setq ,(first outputs)
         (symbol-value ,(first inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:set-symbol-value-instruction) inputs outputs)
  (declare (ignore outputs))
  `(set ,(first inputs) ,(second inputs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on TRANSLATE-BRANCH-INSTRUCTION.

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:eq-instruction) inputs outputs successors)
  `(if (eq ,(first inputs) ,(second inputs))
       (go ,(first successors))
       (go ,(second successors))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:typeq-instruction) inputs outputs successors)
  `(if (typep ,(first inputs) ',(cleavir-ir:value-type instruction))
       (go ,(first successors))
       (go ,(second successors))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnum-add-instruction) inputs outputs successors)
  (let ((result (gensym)))
    `(let ((,result (+ ,(first inputs) ,(second inputs))))
       (cond ((typep result 'fixnum)
	      (setq ,(first outputs) ,result)
	      (go ,(first successors)))
	     ((plusp ,result)
	      (setq ,(first outputs)
		    (+ ,result (* 2 most-negative-fixnum)))
	      (go ,(second successors)))
	     (t
	      (setq ,(first outputs)
		    (- ,result (* 2 most-negative-fixnum)))
	      (go ,(second successors)))))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnum-sub-instruction) inputs outputs successors)
  (let ((result (gensym)))
    `(let ((,result (- ,(first inputs) ,(second inputs))))
       (cond ((typep result 'fixnum)
	      (setq ,(first outputs) ,result)
	      (go ,(first successors)))
	     ((plusp ,result)
	      (setq ,(first outputs)
		    (+ ,result (* 2 most-negative-fixnum)))
	      (go ,(second successors)))
	     (t
	      (setq ,(first outputs)
		    (- ,result (* 2 most-negative-fixnum)))
	      (go ,(second successors)))))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnum-less-instruction) inputs outputs successors)
  (declare (ignore outputs))
  `(if (< ,(first inputs) ,(second inputs))
       (go ,(first successors))
       (go ,(second successors))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnum-not-greater-instruction) inputs outputs successors)
  (declare (ignore outputs))
  `(if (<= ,(first inputs) ,(second inputs))
       (go ,(first successors))
       (go ,(second successors))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnum-equal-instruction) inputs outputs successors)
  (declare (ignore outputs))
  `(if (= ,(first inputs) ,(second inputs))
       (go ,(first successors))
       (go ,(second successors))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:return-instruction) inputs outputs successors)
  (declare (ignore successors))
  `(return (values-list ,(first inputs))))

;;; When the FUNCALL-INSTRUCTION is the last instruction of a basic
;;; block, it is because there is a call to a function that will never
;;; return, such as ERROR, and the instruction then has no successors
;;; (which is why it is at the end of the basic block).
;;;
;;; We therefore must provide a method on TRANSLATE-BRANCH-INSTRUCTION
;;; (in addition to the method on TRANSLATE-SIMPLE-INSTRUCTION)
;;; specialized to FUNCALL-INSTRUCTION.
(defmethod translate-branch-instruction
    ((instruction cleavir-ir:funcall-instruction) inputs outputs successors)
  (declare (ignore outputs successors))
  `(funcall ,(first inputs) ,@(rest inputs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Main entry point.

(defun compile-hir (initial-instruction)
  (check-type initial-instruction cleavir-ir:enter-instruction)
  (compile nil (translate initial-instruction)))

(defun interpret-hir (initial-instruction)
  (funcall (compile-hir initial-instruction)))
