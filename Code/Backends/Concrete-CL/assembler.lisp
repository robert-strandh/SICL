(cl:in-package #:sicl-assembler)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Defining new operations

;;; Allocate opcodes sequentially.
(defparameter *next-available-opcode* 0)

;;; This table just contains an association from numbers to thunks.
;;; As far as the assembler is concerned, it doesn't matter what these
;;; thunks do.  
(defparameter *opcode-to-operation* (make-hash-table :test #'eql))

(defun opcode-to-operation (opcode)
  (let ((operation (gethash opcode *opcode-to-operation*)))
    (if (null operation)
	(error "~s is not a valid opcode" opcode)
	operation)))

;;; To simplify the symbol management, we use upper-case strings for
;;; the name of the operation names.  When opcodes are allocated and
;;; programs are assembled, we systematically translate what we get
;;; into upper-case strings.
(defparameter *opname-to-opcode* (make-hash-table :test #'equalp))

(defun opname-to-opcode (opname)
  (gethash (string-upcase (string opname)) *opname-to-opcode*))

;;; This table allows us to check that a particular operation name
;;; is used with the correct arity. 
(defparameter *opname-to-arity*  (make-hash-table :test #'equalp))

(defun arity-of-opname (opname)
  (gethash (string-upcase (string opname)) *opname-to-arity*))

(defun define-operation (opname arity operation)
  (let ((name (string-upcase (string opname))))
    (setf (gethash name *opname-to-opcode*)
	  *next-available-opcode*)
    (setf (gethash name *opname-to-arity*)
	  arity)
    (setf (gethash *next-available-opcode* *opcode-to-operation*)
	  operation)
    (incf *next-available-opcode*)))

;;; Here an instruction is a list where the first element of the list
;;; is an operation name in the form of a string or a symbol (usually
;;; a symbol).  We verify that the operation name has an associated
;;; operation, and that the length of the list is consistent with the
;;; arity that was declared for this operation. 
(defun check-instruction-syntax (instruction)
  ;; Check that an operation with that name exists
  (when (null (opname-to-opcode (car instruction)))
    (error "no instruction named ~s exists~%" (car instruction)))
  ;; Check that the arity is correct
  (when (/= (1- (length instruction))
	    (arity-of-opname (car instruction)))
    (error "The instruction ~s has ~s arguments but should have ~s."
	   instruction
	   (1- (length instruction))
	   (arity-of-opname (car instruction)))))

(defun check-syntax (instructions)
  (loop for instruction in instructions
	do (check-instruction-syntax instruction)))

;;; Take a list of elements that can be symbolic labels or basic
;;; symbolic instructions with symbolic or numeric arguments.
;;; Translate symbolic arguments that are references to labels into
;;; relative numbers.  Return a list of the translated symbolic
;;; instructions and with the symbolic labels removed.  Instruction
;;; arguments that are symbolic, but do not refer to instruction
;;; labels are preserved.
(defun translate-labels (instructions)
  (let ((labels (make-hash-table :test #'eq)))
    ;; Compute addresses corresponding to
    ;; labels relative to beginning of code. 
    (loop with address = 0
	  for instruction in instructions
	  do (if (symbolp instruction)
		 (setf (gethash instruction labels)
		       address)
		 (incf address (length instruction))))
    ;; Patch instructions with a symbolic argument,
    ;; and remove labels.
    (flet ((translate (thing address)
	     (let ((numeric (gethash thing labels)))
	       (if (null numeric)
		   thing
		   (- numeric address)))))
      (loop with address = 0
	    for instruction in instructions
	    when (consp instruction)
	      collect (progn (incf address (length instruction))
			     (cons (car instruction)
				   (loop for arg in (cdr instruction)
					 collect (translate arg address))))))))

;;; Take a list containing only basic symbolic instructions with only
;;; numeric arguments, and with and return a list of host numbers
;;; corrresponding to the instruction codes and arguments.
(defun symbolic-to-numeric (instructions)
  (loop for instruction in instructions
	collect (opname-to-opcode (car instruction))
	append (cdr instruction)))

;;; Take a list containing symbolic labels and basic symbolic
;;; instructions whose only non-numeric arguments are references to
;;; instruction labels.  Return a list containing the instruction
;;; codes and the numeric instruction arguments.
(defun translate-instructions (instructions)
  (let ((no-labels (translate-labels instructions)))
    (check-syntax no-labels)
    (symbolic-to-numeric no-labels)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Assembly macros

(defparameter *assembly-macros* (make-hash-table :test #'equalp))

(defmacro define-assembly-macro (name lambda-list &body body)
  `(setf (gethash ,(string-upcase (string name)) *assembly-macros*)
	 (lambda ,lambda-list ,@body)))

(defun expand-assembly-macro-form (instruction)
  (let ((fun (gethash (string-upcase (string (car instruction)))
		      *assembly-macros*)))
    (if (null fun)
	(values instruction nil)
	(values (apply fun (cdr instruction)) t))))

(defun expand-all-assembly-macros (instructions)
  (let ((result '()))
    (loop with remaining = instructions
	  until (null remaining)
	  do (let ((first (pop remaining)))
	       (if (symbolp first)
		   (push first result)
		   (multiple-value-bind (expansion expanded-p)
		       (expand-assembly-macro-form first)
		     (if expanded-p
			 (setf remaining (append expansion remaining))
			 (push first result)))))
	  finally (return (nreverse result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Main entry point for the assembler.

(defun assemble (instructions)
  (flet ((collect-constants (instruction)
	   (if (atom instruction)
	       '()
	       (loop for arg in (cdr instruction)
		     when (and (consp arg)
			       (eq (car arg) 'quote))
		       collect (cadr arg))))
	 (patch-instruction (instruction constants)
	   (if (atom instruction)
	       instruction
	       (cons (car instruction)
		     (loop for arg in (cdr instruction)
			   collect (if (and (consp arg)
					    (eq (car arg) 'quote))
				       (position (cadr arg)
						 constants
						 :test #'equalp)
				       arg))))))
    (let* ((expanded-instructions (expand-all-assembly-macros instructions))
	   (constants (loop for instruction in expanded-instructions
			    append (collect-constants instruction)))
	   (augmented-constants (append '(nil) constants))
	   (compressed-constants
	     (remove-duplicates augmented-constants :test #'equalp :from-end t))
	   (instructions-no-constants
	     (loop for instruction in expanded-instructions
		   collect
		   (patch-instruction instruction compressed-constants))))
      (values (translate-instructions instructions-no-constants)
	      compressed-constants))))
