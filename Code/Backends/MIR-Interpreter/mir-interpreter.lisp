(cl:in-package #:sicl-mir-interpreter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Additional instruction to stop the machine.

(defclass halt-instruction (sicl-mir:instruction) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function objects.

(defclass function () ())

(defclass interpreted-function (function)
  ((%instructions :initarg :instructions :reader instructions)
   (%environment :initarg :environment :reader environment)))

(defun make-interpreted-function (instructions environment)
  (make-instance 'interpreted-function
    :instructions instructions
    :environment environment))

(defclass primitive-function (function)
  ((%native-function :initarg :native-function :reader native-function)))

(defun make-primitive-function (native-function)
  (make-instance 'primitive-function
    :native-function native-function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The dynamic environment.

;;; The dynamic environment is a list of instances of DYNAMIC-ENTRY.
;;; The current dynamic environment is stored in a slot in each stack
;;; frame.

(defclass dynamic-entry ()
  ())

;;; A SPECIAL-VARIABLE-ENTRY is a dynamic entry that binds a special
;;; variable to some value. 
(defclass special-variable-entry (dynamic-entry)
  ((%name :initarg :name :reader name)
   (%value :initarg :value :reader value)))

;;; The base class of dynamic entries that store a saved stack that
;;; could be restored later.
(defclass saved-stack-entry (dynamic-entry)
  ((%saved-stack :initarg :saved-stack :reader saved-stack)))

;;; A CATCH-ENTRY is an entry that stores a tag that determines what
;;; stack to restore when used as a target for unwinding.
(defclass catch-entry (saved-stack-entry)
  ((%tag :initarg :tag :reader tag)))

;;; An UNWIND-PROTECT-ENTRY is an entry that stores a function that is
;;; called whenever an attempt is made to unwind beyond this point.
(defclass unwind-protect-entry (dynamic-entry)
  ((%closure :initarg :closure :reader closure)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The stack.

;;; A stack frame contains the following information:
;;;
;;;   * A return address, which is the MIR instruction to execute
;;;     after a RETURN instruction has been executed. 
;;; 
;;;   * A lexical environment.  This environment is a list of vectors. 
;;; 
;;;   * A dynamic environment.  The dynamic environment is a list of
;;;     dynamic entries.
;;;
;;;   * A list of arguments to this function.  We need to store the
;;;     arguments in the stack because parsing the arguments might
;;;     require other function calls, requiring new arguments, etc. 

(defclass stack-frame ()
  ((%arguments
    :initarg :arguments :accessor arguments)
   (%lexical-environment
    :initarg :lexical-environment :accessor lexical-environment)
   (%dynamic-environment
    :initarg :dynamic-environment :accessor dynamic-environment)
   (%executing-function
    :initarg :executing-function :accessor executing-function)
   (%return-address :initarg :return-address :reader return-address)))

;;; The stack is a list of stack frames. 
(defparameter *stack* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Global information.

;;; This variable is used to hold the list of return values from a
;;; function.  It is used by the return instruction in the callee
;;; and by the get-values instruction in the caller. 
(defparameter *values* '())

;;; The currently executing MIR instruction.
(defparameter *program-counter* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reading instruction input and writing instruction output.

(defgeneric read-input (source))

(defmethod read-input ((source sicl-mir:constant-input))
  (sicl-mir:value source))

(defmethod read-input ((source sicl-mir:global-input))
  (car (sicl-mir:storage source)))

;;; FIXME: add method for special location here. 

(defmethod read-input ((source sicl-mir:lexical-location))
  (let ((entry (find source (lexical-environment (car *stack*))
		     :key #'car :test #'eq)))
    (if (null entry)
	(error "Wow, missing entry.  This should not happen!")
	(cdr entry))))

(defun write-output (destination value)
  (let ((entry (find destination (lexical-environment (car *stack*))
		     :key #'car :test #'eq)))
    (if (null entry)
	(push (cons destination value)
	      (lexical-environment (car *stack*)))
	(setf (cdr entry) value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Executing instructions.

(defgeneric execute-instruction (instruction))

(defmethod execute-instruction
    ((instruction sicl-mir:get-argcount-instruction))
  (write-output (car (sicl-mir:outputs instruction))
		(length (arguments (car *stack*))))
  (setf *program-counter* (car (sicl-mir:successors instruction))))

(defmethod execute-instruction
    ((instruction sicl-mir:get-arg-instruction))
  (write-output (car (sicl-mir:outputs instruction))
		(elt (arguments (car *stack*))
		     (read-input (car (sicl-mir:inputs instruction)))))
  (setf *program-counter* (car (sicl-mir:successors instruction))))

(defmethod execute-instruction
    ((instruction sicl-mir:assignment-instruction))
  (write-output (car (sicl-mir:outputs instruction))
		(read-input (car (sicl-mir:inputs instruction))))
  (setf *program-counter* (car (sicl-mir:successors instruction))))

(defmethod execute-instruction
    ((instruction sicl-mir:funcall-instruction))
  (let ((fun (read-input (car (sicl-mir:inputs instruction))))
	(args (loop for input in (cdr (sicl-mir:inputs instruction))
		    collect (read-input input))))
    (if (typep fun 'interpreted-function)
	(progn 
	  (push (make-instance 'stack-frame
		  :arguments args
		  :lexical-environment (environment fun)
		  :executing-function fun
		  :return-address (car (sicl-mir:successors instruction)))
		*stack*)
	  (setf *program-counter*
		(instructions fun)))
	(progn
	  (setf *values*
		(multiple-value-list
		 (apply (native-function fun) args)))
	  (setf *program-counter* (car (sicl-mir:successors instruction)))))))

(defmethod execute-instruction
    ((instruction sicl-mir:tailcall-instruction))
  (let ((fun (read-input (car (sicl-mir:inputs instruction))))
	(args (loop for input in (cdr (sicl-mir:inputs instruction))
		    collect (read-input input))))
    (if (typep fun 'interpreted-function)
	(progn 
	  (setf (arguments (car *stack*)) args
		(lexical-environment (car *stack*)) (environment fun)
		(executing-function (car *stack*)) fun)
	  (setf *program-counter*
		(instructions fun)))
	(progn
	  (setf *values*
		(multiple-value-list
		 (apply (native-function fun) args)))
	  (setf *program-counter*
		(return-address (car *stack*)))
	  (pop *stack*)))))

(defmethod execute-instruction
    ((instruction sicl-mir:return-instruction))
  (setf *values*
	(loop for arg in (sicl-mir:inputs instruction)
	      collect (read-input arg)))
  (setf *program-counter*
	(return-address (car *stack*)))
  (pop *stack*))

(defmethod execute-instruction
    ((instruction sicl-mir:get-values-instruction))
  (let ((outputs (sicl-mir:outputs instruction)))
    ;; Initialize all the outputs to NIL in case there are fewer
    ;; return values than outputs.
    (loop for output in outputs
	  do (write-output output nil))
    ;; Write return values to outputs.
    (loop for value in *values*
	  for output in outputs
	  do (write-output output value)))
  (setf *program-counter* (car (sicl-mir:successors instruction))))

(defmethod execute-instruction
    ((instruction sicl-mir:nop-instruction))
  (setf *program-counter* (car (sicl-mir:successors instruction))))

(defmethod execute-instruction ((instruction sicl-mir:==-instruction))
  (setf *program-counter*
	(if (eq (read-input (car (sicl-mir:inputs instruction)))
		(read-input (cadr (sicl-mir:inputs instruction))))
	    (car (sicl-mir:successors instruction))
	    (cadr (sicl-mir:successors instruction)))))

(defmethod execute-instruction ((instruction sicl-mir:enclose-instruction))
  (write-output (car (sicl-mir:outputs instruction))
		(make-interpreted-function
		 (sicl-mir:code instruction)
		 (lexical-environment (car *stack*))))
  (setf *program-counter* (car (sicl-mir:successors instruction))))


(defmethod execute-instruction ((instruction halt-instruction))
  (throw 'halt *values*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Function EVAL.

(defun initialize-function (name function)
  (let* ((fun (make-primitive-function function))
	 (location (make-instance 'sicl-env::global-location
		     :name name
		     :storage (list fun)))
	 (entry (make-instance 'sicl-env::global-function-entry
		  :name name
		  :location location)))
    (push entry
	  (sicl-env::functions sicl-env:*global-environment*))))

(defun initialize-named-function (name function-name)
  (initialize-function name (cl:fdefinition function-name)))

(defun initialize-environment ()
  (loop for name in '(> < + - error)
	do (initialize-named-function name name)))

(defun eval (form)
  (let* ((ast (sicl-compiler-phase-1:convert-top-level-form form))
	 (mir (sicl-compiler-phase-2:compile-toplevel ast))
	 (fun (make-interpreted-function mir '())))
    (setf *stack*
	  (list (make-instance 'stack-frame
		  :arguments '()
		  :lexical-environment '()
		  :executing-function fun
		  :return-address (make-instance 'halt-instruction))))
    (setf *program-counter* mir)
    (catch 'halt
      (loop do (execute-instruction *program-counter*)))))
		  
