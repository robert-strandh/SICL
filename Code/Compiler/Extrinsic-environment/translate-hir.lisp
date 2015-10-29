(cl:in-package #:sicl-extrinsic-environment)

(defclass fun (closer-mop:funcallable-standard-object)
  ((%untied :initform nil :initarg :untied :accessor untied)
   (%arg-forms :initform nil :initarg :arg-forms :accessor arg-forms)
   (%name :initform NIL :accessor name))
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod print-object ((object fun) stream)
  (print-unreadable-object (object stream)
    (format stream "Host function named: ~s" (name object))))

(defmethod (setf sicl-genv:fdefinition)
    :after ((new-definition fun) function-name environment)
  (setf (name new-definition) function-name))

;;; The first argument to this function is an instruction that has a
;;; single successor.  Whether a GO is required at the end of this
;;; function is determined by the code layout algorithm.  
;;; 
;;; The inputs are forms to be evaluated.  The outputs are symbols
;;; that are names of variables.  STATIC-ENVIRONMENT is a list of
;;; symbols.
(defgeneric translate-simple-instruction
    (instruction inputs outputs static-environment))

(defgeneric translate-branch-instruction (instruction inputs outputs successors))

(defvar *basic-blocks*)

;;; This variable holds an EQ hash table mapping each instruction
;;; to its owner.  The owner is an ENTER-INSTRUCTION.
(defvar *instruction-ownerships*)

;;; This variable holds an EQ hash table mapping each datum to its
;;; owner.  The owner is an ENTER-INSTRUCTION.
(defvar *location-ownerships*)

;;; For a given owner (which can be an ENTER-INSTRUCTION or NIL),
;;; return a list of all the variables (lexical or values) that are
;;; owned by that instruction.
(defun compute-owned-variables (owner)
  (let ((function-p (typep owner 'cleavir-ir:enter-instruction)))
    (loop for var being each hash-key of *location-ownerships*
	    using (hash-value value)
	  when (and (typep var '(or
				 cleavir-ir:lexical-location
				 cleavir-ir:values-location))
		    (eq value (if function-p
				  owner
				  nil)))
	    collect (translate-datum var))))

(defvar *tags*)
(defvar *vars*)

;;; This variable holds an EQ hash table, mapping each
;;; ENTER-INSTRUCTION to a GENSYMed symbol that is used to hold the
;;; dynamic environment as it was when the function corresponding to
;;; that ENTER-INSTRUCTION was called.
(defvar *dynamic-environment-variables*)

(defun translate-datum (datum)
  (let ((var (gethash datum *vars*)))
    (when (null var)
      (setf var (gensym))
      (setf (gethash datum *vars*) var))
    var))

(defun translate-lambda-list (lambda-list)
  (loop for item in lambda-list
	collect (cond ((member item lambda-list-keywords)
		       item)
		      ((and (listp item) (= (length item) 2))
		       (list (translate-datum (first item))
			     (translate-datum (second item))))
		      ((and (listp item) (= (length item) 3))
		       (list (first item)
			     (translate-datum (second item))
			     (translate-datum (third item))))
		      (t
		       (translate-datum item)))))

(defun layout-basic-block (basic-block static-environment)
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
			   instruction input-vars output-vars
			   static-environment))
	    (let* ((inputs (cleavir-ir:inputs last))
		   (input-vars (mapcar #'translate-datum inputs))
		   (outputs (cleavir-ir:outputs last))
		   (output-vars (mapcar #'translate-datum outputs))
		   (successors (cleavir-ir:successors last))
		   (successor-tags (loop for successor in successors
					 collect (gethash successor *tags*))))
	      (if (= (length successors) 1)
		  (list (translate-simple-instruction
			 last input-vars output-vars static-environment)
			`(go ,(gethash (first successors) *tags*)))
		  (list (translate-branch-instruction
			 last input-vars output-vars successor-tags)))))))

(defun layout-procedure (initial-instruction)
  ;; Make sure we have an ENTER-INSTRUCTION.
  (assert (typep initial-instruction 'cleavir-ir:enter-instruction))
  ;; Make sure we do not already have a variable associated with this
  ;; ENTER-INSTRUCTION for holding the dynamic environment at runtime.
  (assert (null (gethash initial-instruction *dynamic-environment-variables*)))
  (let* (;; Generate a new variable for holding the dynamic
	 ;; environment at runtime.
	 (dynamic-environment-variable (gensym))
	 (basic-blocks (remove initial-instruction
			       *basic-blocks*
			       :test-not #'eq :key #'third))
	 (first (find initial-instruction basic-blocks
		      :test #'eq :key #'first))
	 (rest (remove first basic-blocks :test #'eq)))
    (setf (gethash initial-instruction *dynamic-environment-variables*)
	  dynamic-environment-variable)
    ;; Assign tags to all basic block except the first one
    (loop for block in rest
	  for instruction = (first block)
	  do (setf (gethash instruction *tags*) (gensym)))
    (let ((tagbody
	     `(tagbody
		 ,@(layout-basic-block first nil)
		 ,@(loop for basic-block in rest
			 collect (gethash (first basic-block) *tags*)
			 append (layout-basic-block basic-block nil))))
	  (owned-vars (compute-owned-variables initial-instruction)))
      `(lambda (&rest args)
	 (block nil
	   (let ((,dynamic-environment-variable *dynamic-environment*)
		 ,@owned-vars)
	     (declare (ignorable ,dynamic-environment-variable
				 ,@owned-vars))
	     ,(build-argument-parsing-code
	       (translate-lambda-list
		(cleavir-ir:lambda-list initial-instruction))
	       'args)
	     ,tagbody))))))

(defparameter *hir-to-cl-translation-meter*
  (make-instance 'cleavir-meter:basic-meter
    :name "HIR-to-Common-Lisp translation"))

(defun translate (initial-instruction linkage-environment)
  (cleavir-meter:with-meter (m *hir-to-cl-translation-meter*)
    (let ((*instruction-ownerships*
	    (cleavir-hir-transformations:compute-instruction-owners
	     initial-instruction))
	  (*location-ownerships*
	    (cleavir-hir-transformations:compute-location-owners
	     initial-instruction))
	  (*basic-blocks* (cleavir-basic-blocks:basic-blocks initial-instruction))
	  (*linkage-environment* linkage-environment)
	  (*tags* (make-hash-table :test #'eq))
	  (*vars* (make-hash-table :test #'eq))
	  (*dynamic-environment-variables* (make-hash-table :test #'eq)))
      (layout-procedure initial-instruction))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on TRANSLATE-SIMPLE-INSTRUCTION.

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:enclose-instruction)
     inputs outputs static-environment)
  (declare (ignore static-environment))
  (declare (ignore inputs))
  (let* ((enter-instruction (cleavir-ir:code instruction))
	 (temp (gensym))
	 (proc (layout-procedure enter-instruction)))
    `(setq ,(first outputs)
	   (let ((,temp (make-instance 'fun)))
	     (closer-mop:set-funcallable-instance-function ,temp ,proc)
	     ,temp))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:enter-instruction)
     inputs outputs static-environment)
  (declare (ignore static-environment))
  (declare (ignore inputs outputs))
  (gensym))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:assignment-instruction)
     inputs outputs static-environment)
  (declare (ignore static-environment))
  `(setq ,(first outputs) ,(first inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:funcall-instruction)
     inputs outputs static-environment)
  (declare (ignore static-environment))
  (let* ((owner (gethash instruction *instruction-ownerships*))
	 (var (gethash owner *dynamic-environment-variables*)))
    `(progn (setf *dynamic-environment* ,var)
	    (setf ,(first outputs)
		  (multiple-value-list (traced-funcall
					,*linkage-environment*
					,(first inputs)
					,@(rest inputs)))))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:multiple-value-call-instruction)
     inputs outputs static-environment)
  (declare (ignore static-environment))
  `(setf ,(first outputs)
	 (multiple-value-list
	  (apply ,(first inputs)
		 (append ,@(rest inputs))))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:tailcall-instruction)
     inputs outputs static-environment)
  (declare (ignore static-environment))
  (declare (ignore outputs))
  (let* ((owner (gethash instruction *instruction-ownerships*))
	 (var (gethash owner *dynamic-environment-variables*)))
    ;; It is not quite correct to set the dynamic environment to that
    ;; of the current function.  Since it is a tail call it should
    ;; really be set to the dynamic environment of the caller.
    ;; However, no function translated here will alter its dynamic
    ;; run-time environment in any way.  Such modifications are only
    ;; done in host functions.
    `(progn (setf *dynamic-environment* ,var)
	    (return (traced-funcall
		     ,*linkage-environment*
		     ,(first inputs)
		     ,@(rest inputs))))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:the-instruction)
     inputs outputs static-environment)
  (declare (ignore static-environment))
  (declare (ignore outputs))
  `(unless (traced-funcall
	    ,*linkage-environment*
	    (car (load-time-value
		  (sicl-env:function-cell
		   'typep
		   ,*linkage-environment*)
		  nil))
	    ,(first inputs)
	    ',(cleavir-ir:value-type instruction))
     (traced-funcall
      ,*linkage-environment*
      (car (load-time-value
	    (sicl-env:function-cell
	     'error
	     ,*linkage-environment*)
	    nil))
      'type-error
      :expected-type ',(cleavir-ir:value-type instruction)
      :datum ,(first inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:car-instruction)
     inputs outputs static-environment)
  (declare (ignore static-environment))
  `(setq ,(first outputs)
	 (car ,(first inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:cdr-instruction)
     inputs outputs static-environment)
  (declare (ignore static-environment))
  `(setq ,(first outputs)
	 (cdr ,(first inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:rplaca-instruction)
     inputs outputs static-environment)
  (declare (ignore static-environment))
  (declare (ignore outputs))
  `(rplaca ,(first inputs) ,(second inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:rplacd-instruction)
     inputs outputs static-environment)
  (declare (ignore static-environment))
  (declare (ignore outputs))
  `(rplacd ,(first inputs) ,(second inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:t-aref-instruction)
     inputs outputs static-environment)
  (declare (ignore static-environment))
  `(setq ,(first outputs)
	 (row-major-aref ,(first inputs) ,(second inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:bit-aref-instruction)
     inputs outputs static-environment)
  (declare (ignore static-environment))
  `(setq ,(first outputs)
	 (row-major-aref ,(first inputs) ,(second inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:unsigned-byte-8-aref-instruction)
     inputs outputs static-environment)
  (declare (ignore static-environment))
  `(setq ,(first outputs)
	 (row-major-aref ,(first inputs) ,(second inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:short-float-aref-instruction)
     inputs outputs static-environment)
  (declare (ignore static-environment))
  `(setq ,(first outputs)
	 (row-major-aref ,(first inputs) ,(second inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:single-float-aref-instruction)
     inputs outputs static-environment)
  (declare (ignore static-environment))
  `(setq ,(first outputs)
	 (row-major-aref ,(first inputs) ,(second inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:double-float-aref-instruction)
     inputs outputs static-environment)
  (declare (ignore static-environment))
  `(setq ,(first outputs)
	 (row-major-aref ,(first inputs) ,(second inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:long-float-aref-instruction)
     inputs outputs static-environment)
  (declare (ignore static-environment))
  `(setq ,(first outputs)
	 (row-major-aref ,(first inputs) ,(second inputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:t-aset-instruction)
     inputs outputs static-environment)
  (declare (ignore static-environment))
  (declare (ignore outputs))
  `(setf (row-major-aref ,(first inputs) ,(second inputs))
	 ,(third inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:bit-aset-instruction)
     inputs outputs static-environment)
  (declare (ignore static-environment))
  (declare (ignore outputs))
  `(setf (row-major-aref ,(first inputs) ,(second inputs))
	 ,(third inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:unsigned-byte-8-aset-instruction)
     inputs outputs static-environment)
  (declare (ignore static-environment))
  (declare (ignore outputs))
  `(setf (row-major-aref ,(first inputs) ,(second inputs))
	 ,(third inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:short-float-aset-instruction)
     inputs outputs static-environment)
  (declare (ignore static-environment))
  (declare (ignore outputs))
  `(setf (row-major-aref ,(first inputs) ,(second inputs))
	 ,(third inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:single-float-aset-instruction)
     inputs outputs static-environment)
  (declare (ignore static-environment))
  (declare (ignore outputs))
  `(setf (row-major-aref ,(first inputs) ,(second inputs))
	 ,(third inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:double-float-aset-instruction)
     inputs outputs static-environment)
  (declare (ignore static-environment))
  (declare (ignore outputs))
  `(setf (row-major-aref ,(first inputs) ,(second inputs))
	 ,(third inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:long-float-aset-instruction)
     inputs outputs static-environment)
  (declare (ignore static-environment))
  (declare (ignore outputs))
  `(setf (row-major-aref ,(first inputs) ,(second inputs))
	 ,(third inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:fdefinition-instruction)
     inputs outputs static-environment)
  (declare (ignore static-environment))
  `(setf ,(first outputs)
	 (car (load-time-value
	       (sicl-env:function-cell ,(first inputs)
				       ,*linkage-environment*)
	       nil))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:fixed-to-multiple-instruction)
     inputs outputs static-environment)
  (declare (ignore static-environment))
  `(setq ,(first outputs)
	 (list ,@inputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:multiple-to-fixed-instruction)
     inputs outputs static-environment)
  (declare (ignore static-environment))
  (let ((temp (gensym)))
    `(let ((,temp ,(first inputs)))
       (declare (ignorable ,temp))
       ,@(loop for output in outputs
	       collect `(setf ,output (pop ,temp))))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:nop-instruction)
     inputs outputs static-environment)
  (declare (ignore static-environment))
  (declare (ignore inputs outputs))
  '(progn))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:unwind-instruction)
     inputs outputs static-environment)
  (declare (ignore static-environment))
  (declare (ignore inputs outputs))
  (let ((dynamic-environment-variable
	  (gethash (cleavir-ir:invocation instruction)
		   *dynamic-environment-variables*)))
    `(unwind ,dynamic-environment-variable)))

;;; Recall that the FETCH-INSTRUCTION fetches a CELL from the static
;;; environment.  The instruction has two inputs and one output.  The
;;; first input is a lexical variable holding the static environment
;;; and the second is an immediate number that serves as an index into
;;; the static environment.  However, here, we do not represent the
;;; static environment as a vector at run-time, because then we would
;;; have to allocate that vector when we create a closure.  Instead,
;;; we represent the static environment at compile-time as a list of
;;; symbols representing host variables.  The second input of the
;;; instruction will indicate the element in the list to be used.
(defmethod translate-simple-instruction
    ((instruction cleavir-ir:fetch-instruction)
     inputs outputs static-environement)
  `(setq ,(first outputs)
	 ,(second inputs)))

;;; Recall that the READ-CELL instruction has a single input and a
;;; single output.  The input is a lexical location holding a CELL and
;;; the output is the contents of the cell.  Since we represent CELLs
;;; as CONSes, this instruction is implemented by taking the CAR of
;;; the cell.
(defmethod translate-simple-instruction
    ((instruction cleavir-ir:read-cell-instruction)
     inputs outputs static-environement)
  `(setq ,(first outputs)
	 (car ,(first inputs))))

;;; Recall that the WRITE-CELL instruction has two inputs.  The first
;;; input is a lexical location holding a CELL and the second input is
;;; the value to be written to the cell.  Since we represent CELLs as
;;; CONSes, this instruction is implemented by SETF-ing the CAR of the
;;; CONS cell.
(defmethod translate-simple-instruction
    ((instruction cleavir-ir:write-cell-instruction)
     inputs outputs static-environement)
  `(setq (car ,(first inputs))
	 ,(second inputs)))

;;; Recall that the CREATE-CELL instruction has a single output,
;;; namely a lexical location that will hold the CELL that is being
;;; created by the instruction.  Since we represent cells as CONSes,
;;; this instruction is implemented by calling LIST to allocate a list
;;; with a single element NIL and assigning that list to the lexical
;;; location.
(defmethod translate-simple-instruction
    ((instruction cleavir-ir:create-cell-instruction)
     inputs outputs static-environement)
  `(setq ,(first outputs)
	 (list nil)))

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
  `(if (traced-funcall
	,*linkage-environment*
	(car (load-time-value
	      (sicl-env:function-cell
	       'typep
	       ,*linkage-environment*)
	      nil))
	,(first inputs)
	',(cleavir-ir:value-type instruction))
       (go ,(second successors))
       (go ,(first successors))))

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
  `(return (apply #'values ,(first inputs))))

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
  (let* ((owner (gethash instruction *instruction-ownerships*))
	 (var (gethash owner *dynamic-environment-variables*)))
    `(progn (setf *dynamic-environment* ,var)
	    (traced-funcall
	     ,*linkage-environment*
	     ,(first inputs)
	     ,@(rest inputs)))))
