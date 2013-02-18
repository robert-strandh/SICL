(in-package #:sicl-mir)

(defclass instruction ()
  ((%successors :initform '() :initarg :successors :accessor successors)
   (%inputs :initform '() :initarg :inputs :reader inputs)
   (%outputs :initform '() :initarg :outputs :reader outputs)))

(defmethod initialize-instance :after ((obj instruction) &key &allow-other-keys)
  (unless (and (listp (successors obj))
	       (every (lambda (successor)
			(typep successor 'instruction))
		      (successors obj)))
    (error "successors must be a list of instructions")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing instructions.

(defparameter *instruction-table* nil)

(defgeneric draw-instruction (instruction stream))
  
(defmethod draw-instruction :around (instruction stream)
  (when (null (gethash instruction *instruction-table*))
    (setf (gethash instruction *instruction-table*) (gensym))
    (format stream "  ~a [shape = box];~%"
	    (gethash instruction *instruction-table*))
    (call-next-method)))

(defmethod draw-instruction :before ((instruction instruction) stream)
  (loop for next in (successors instruction)
	do (draw-instruction next stream))
  (loop for next in (successors instruction)
	do (format stream
		   "  ~a -> ~a [style = bold];~%"
		   (gethash instruction *instruction-table*)
		   (gethash next *instruction-table*))))
  
(defmethod draw-instruction (instruction stream)
  (format stream "   ~a [label = \"~a\"];~%"
	  (gethash instruction *instruction-table*)
	  (class-name (class-of instruction))))

(defgeneric draw-location (location stream))

(defmethod draw-location :around (location stream)
  (when (null (gethash location *instruction-table*))
    (setf (gethash location *instruction-table*) (gensym))
    (format stream
	    "  ~a [shape = ellipse, style = filled, label = \"~a\"];~%"
	    (gethash location *instruction-table*)
	    (sicl-env:name location))
    (call-next-method)))

(defmethod draw-location ((location sicl-env:global-location) stream)
  (format stream "   ~a [fillcolor = green];~%"
	  (gethash location *instruction-table*)))

(defmethod draw-location ((location sicl-env:lexical-location) stream)
  (format stream "   ~a [fillcolor = yellow];~%"
	  (gethash location *instruction-table*)))

(defun draw-location-info (info stream)
  (when (null (gethash info *instruction-table*))
    (setf (gethash info *instruction-table*) (gensym))
    (draw-location (sicl-env:location info) stream)
    (format stream "  ~a [shape = box, label = \"~a\"]~%" 
	    (gethash info *instruction-table*)
	    (sicl-env:name (sicl-env:location info)))
    (format stream "  ~a -> ~a [color = green]~%"
	    (gethash info *instruction-table*)
	    (gethash (sicl-env:location info) *instruction-table*))))

(defmethod draw-instruction :after (instruction stream)
  (loop for location in (inputs instruction)
	do (draw-location-info location stream)
	   (format stream "  ~a -> ~a [color = red, style = dashed];~%"
		   (gethash location *instruction-table*)
		   (gethash instruction *instruction-table*)))
  (loop for location in (outputs instruction)
	do (draw-location-info location stream)
	   (format stream "  ~a -> ~a [color = blue, style = dashed];~%"
		   (gethash instruction *instruction-table*)
		   (gethash location *instruction-table*))))

(defun draw-flowchart (start filename)
  (with-open-file (stream filename
			  :direction :output
			  :if-exists :supersede)
    (let ((*instruction-table* (make-hash-table :test #'eq)))
	(format stream "digraph G {~%")
	(draw-instruction start stream)
	(format stream "}~%"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instructions for Common Lisp operators.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction END-INSTRUCTION.

(defclass end-instruction (instruction)
  ())

(defun make-end-instruction ()
  (make-instance 'end-instruction))

(defmethod draw-instruction ((instruction end-instruction) stream)
  (format stream "   ~a [label = \"end\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction NOP-INSTRUCTION.

(defclass nop-instruction (instruction)
  ())

(defun make-nop-instruction (successors)
  (make-instance 'nop-instruction
    :successors successors))

(defmethod draw-instruction ((instruction nop-instruction) stream)
  (format stream "   ~a [label = \"nop\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction CONSTANT-ASSIGNMENT-INSTRUCTION.

(defclass constant-assignment-instruction (instruction)
  ((%constant :initarg :constant :accessor constant)))

(defun make-constant-assignment-instruction (output successor constant)
  (make-instance 'constant-assignment-instruction
    :outputs (list output)
    :successors (list successor)
    :constant constant))

(defmethod draw-instruction
    ((instruction constant-assignment-instruction) stream)
  (format stream "   ~a [label = \"<=\"];~%"
	  (gethash instruction *instruction-table*))
  (let ((name (gensym)))
    (format stream "   ~a [label = \"~a\", style = filled, fillcolor = pink];~%"
	    name
	    (constant instruction))
    (format stream "   ~a [fillcolor = pink];~%"
	    name)
    (format stream "   ~a -> ~a [color = pink, style = dashed];~%"
	    name
	    (gethash instruction *instruction-table*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction VARIABLE-ASSIGNMENT-INSTRUCTION.

(defclass variable-assignment-instruction (instruction)
  ())

(defun make-variable-assignment-instruction (input output successor)
  (make-instance 'variable-assignment-instruction
    :inputs (list input)
    :outputs (list output)
    :successors (list successor)))

(defmethod draw-instruction
    ((instruction variable-assignment-instruction) stream)
  (format stream "   ~a [label = \"<-\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction TEST-INSTRUCTION.

(defclass test-instruction (instruction)
  ())

(defun make-test-instruction (input successors)
  (make-instance 'test-instruction
    :inputs (list input)
    :successors successors))

(defmethod draw-instruction ((instruction test-instruction) stream)
  (format stream "   ~a [label = \"test\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction FUNCALL-INSTRUCTION.

(defclass funcall-instruction (instruction)
  ((%fun :initarg :fun :accessor fun)))

(defun make-funcall-instruction (successor fun)
  (make-instance 'funcall-instruction
    :successors (list successor)
    :fun fun))

(defmethod draw-instruction ((instruction funcall-instruction) stream)
  (draw-location-info (fun instruction) stream)
  (format stream "   ~a [label = \"funcall\"];~%"
	  (gethash instruction *instruction-table*))
  (format stream "   ~a -> ~a [color = red, style = dashed];~%"
	  (gethash (fun instruction) *instruction-table*)
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction GET-ARGUMENTS-INSTRUCTION.

(defclass get-arguments-instruction (instruction)
  ((%lambda-list :initarg :lambda-list :accessor lambda-list)))

(defun make-get-arguments-instruction (successor lambda-list)
  (make-instance 'get-arguments-instruction
    :successors (list successor)
    :lambda-list lambda-list))

(defmethod outputs ((instruction get-arguments-instruction))
  (sicl-ast:required (lambda-list instruction)))

(defmethod draw-instruction ((instruction get-arguments-instruction) stream)
  (format stream "   ~a [label = \"get-args\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction GET-VALUES-INSTRUCTION.

(defclass get-values-instruction (instruction)
  ())

(defun make-get-values-instruction (outputs successor)
  (make-instance 'get-values-instruction
    :outputs outputs
    :successors (list successor)))

(defmethod draw-instruction ((instruction get-values-instruction) stream)
  (format stream "   ~a [label = \"get-values\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction PUT-ARGUMENTS-INSTRUCTION.

(defclass put-arguments-instruction (instruction)
  ())

(defun make-put-arguments-instruction (inputs successor)
  (make-instance 'put-arguments-instruction
    :inputs inputs
    :successors (list successor)))

(defmethod draw-instruction ((instruction put-arguments-instruction) stream)
  (format stream "   ~a [label = \"put-args\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction PUT-VALUES-INSTRUCTION.

(defclass put-values-instruction (instruction)
  ())

(defun make-put-values-instruction (inputs successor)
  (make-instance 'put-values-instruction
    :inputs inputs
    :successors (list successor)))

(defmethod draw-instruction ((instruction put-values-instruction) stream)
  (format stream "   ~a [label = \"put-values\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction ENTER-INSTRUCTION.
;;;
;;; FIXME: maybe remove.

(defclass enter-instruction (instruction)
  ())

(defmethod draw-instruction ((instruction enter-instruction) stream)
  (format stream "   ~a [label = \"enter\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction LEAVE-INSTRUCTION.
;;;
;;; FIXME: maybe remove.

(defclass leave-instruction (instruction)
  ())

(defmethod draw-instruction ((instruction leave-instruction) stream)
  (format stream "   ~a [label = \"leave\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction RETURN-INSTRUCTION.

(defclass return-instruction (instruction)
  ())

(defun make-return-instruction ()
  (make-instance 'return-instruction))

(defmethod draw-instruction ((instruction return-instruction) stream)
  (format stream "   ~a [label = \"ret\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction ENCLOSE-INSTRUCTION.

(defclass enclose-instruction (instruction)
  ((%code :initarg :code :accessor code)))  

(defun make-enclose-instruction (output successor code)
  (make-instance 'enclose-instruction
    :outputs (list output)
    :successors (list successor)
    :code code))

(defmethod draw-instruction ((instruction enclose-instruction) stream)
  (format stream "   ~a [label = \"enclose\"];~%"
	  (gethash instruction *instruction-table*))
  (draw-instruction (code instruction) stream)
  (format stream "  ~a -> ~a [color = pink, style = dashed];~%"
	  (gethash (code instruction) *instruction-table*)
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instructions for low-level operators.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction MEMALLOC-INSTRUCTION.

(defclass memalloc-instruction (instruction)
  ())

(defun make-memalloc-instruction (input output successor)
  (make-instance 'memalloc-instruction
    :inputs (list input)
    :outputs (list output)
    :successors (list successor)))

(defmethod draw-instruction ((instruction memalloc-instruction) stream)
  (format stream "   ~a [label = \"memalloc\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction MEMREF-INSTRUCTION.

(defclass memref-instruction (instruction)
  ())

(defun make-memref-instruction (input output successor)
  (make-instance 'memref-instruction
    :inputs (list input)
    :outputs (list output)
    :successors (list successor)))

(defmethod draw-instruction ((instruction memref-instruction) stream)
  (format stream "   ~a [label = \"memref\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction MEMSET-INSTRUCTION.

(defclass memset-instruction (instruction)
  ())

(defun make-memset-instruction (inputs successor)
  (make-instance 'memset-instruction
    :inputs inputs
    :successors (list successor)))

(defmethod draw-instruction ((instruction memset-instruction) stream)
  (format stream "   ~a [label = \"memset\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction U+-INSTRUCTION.

(defclass u+-instruction (instruction)
  ())

(defun make-u+-instruction (inputs output successors)
  (make-instance 'u+-instruction
    :inputs inputs
    :outputs (list output)
    :successors successors))

(defmethod draw-instruction ((instruction u+-instruction) stream)
  (format stream "   ~a [label = \"u+\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction U--INSTRUCTION.

(defclass u--instruction (instruction)
  ())

(defun make-u--instruction (inputs output successors)
  (make-instance 'u--instruction
    :inputs inputs
    :outputs (list output)
    :successors successors))

(defmethod draw-instruction ((instruction u--instruction) stream)
  (format stream "   ~a [label = \"u-\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction S+-INSTRUCTION.

(defclass s+-instruction (instruction)
  ())

(defun make-s+-instruction (inputs output successors)
  (make-instance 's+-instruction
    :inputs inputs
    :outputs (list output)
    :successors successors))

(defmethod draw-instruction ((instruction s+-instruction) stream)
  (format stream "   ~a [label = \"s+\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction S--INSTRUCTION.

(defclass s--instruction (instruction)
  ())

(defun make-s--instruction (inputs output successors)
  (make-instance 's--instruction
    :inputs inputs
    :outputs (list output)
    :successors successors))

(defmethod draw-instruction ((instruction s--instruction) stream)
  (format stream "   ~a [label = \"s-\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction NEG-INSTRUCTION.

(defclass neg-instruction (instruction)
  ())

(defun make-neg-instruction (inputs output successors)
  (make-instance 'neg-instruction
    :inputs inputs
    :outputs (list output)
    :successors successors))

(defmethod draw-instruction ((instruction neg-instruction) stream)
  (format stream "   ~a [label = \"neg\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction &-INSTRUCTION.

(defclass &-instruction (instruction)
  ())

(defun make-&-instruction (inputs output successor)
  (make-instance '&-instruction
    :inputs inputs
    :outputs (list output)
    :successors (list successor)))

(defmethod draw-instruction ((instruction &-instruction) stream)
  (format stream "   ~a [label = \"&\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction IOR-INSTRUCTION.

(defclass ior-instruction (instruction)
  ())

(defun make-ior-instruction (inputs output successor)
  (make-instance 'ior-instruction
    :inputs inputs
    :outputs (list output)
    :successors (list successor)))

(defmethod draw-instruction ((instruction ior-instruction) stream)
  (format stream "   ~a [label = \"ior\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction XOR-INSTRUCTION.

(defclass xor-instruction (instruction)
  ())

(defun make-xor-instruction (inputs output successor)
  (make-instance 'xor-instruction
    :inputs inputs
    :outputs (list output)
    :successors (list successor)))

(defmethod draw-instruction ((instruction xor-instruction) stream)
  (format stream "   ~a [label = \"xor\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction ~-INSTRUCTION.

(defclass ~-instruction (instruction)
  ())

(defun make-~-instruction (inputs output successor)
  (make-instance '~-instruction
    :inputs inputs
    :outputs (list output)
    :successors (list successor)))

(defmethod draw-instruction ((instruction ~-instruction) stream)
  (format stream "   ~a [label = \"~\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction ==-INSTRUCTION.

(defclass ==-instruction (instruction)
  ())

(defun make-==-instruction (inputs successors)
  (make-instance '==-instruction
    :inputs inputs
    :successors successors))

(defmethod draw-instruction ((instruction ==-instruction) stream)
  (format stream "   ~a [label = \"==\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction S<-INSTRUCTION.

(defclass s<-instruction (instruction)
  ())

(defun make-s<-instruction (inputs successors)
  (make-instance 's<-instruction
    :inputs inputs
    :successors successors))

(defmethod draw-instruction ((instruction s<-instruction) stream)
  (format stream "   ~a [label = \"s<\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction S<=-INSTRUCTION.

(defclass s<=-instruction (instruction)
  ())

(defun make-s<=-instruction (inputs successors)
  (make-instance 's<=-instruction
    :inputs inputs
    :successors successors))

(defmethod draw-instruction ((instruction s<=-instruction) stream)
  (format stream "   ~a [label = \"s<=\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction U<-INSTRUCTION.

(defclass u<-instruction (instruction)
  ())

(defun make-u<-instruction (inputs successors)
  (make-instance 'u<-instruction
    :inputs inputs
    :successors successors))

(defmethod draw-instruction ((instruction u<-instruction) stream)
  (format stream "   ~a [label = \"u<\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction U<=-INSTRUCTION.

(defclass u<=-instruction (instruction)
  ())

(defun make-u<=-instruction (inputs successors)
  (make-instance 'u<=-instruction
    :inputs inputs
    :successors successors))

(defmethod draw-instruction ((instruction u<=-instruction) stream)
  (format stream "   ~a [label = \"u<=\"];~%"
	  (gethash instruction *instruction-table*)))
