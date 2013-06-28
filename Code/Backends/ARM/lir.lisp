(in-package #:sicl-arm-lir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The ARM backend.

(defvar *registers*
  (vector (sicl-mir:make-register-location 'sicl-arm-assembler:r0)
	  (sicl-mir:make-register-location 'sicl-arm-assembler:r1)
	  (sicl-mir:make-register-location 'sicl-arm-assembler:r2)
	  (sicl-mir:make-register-location 'sicl-arm-assembler:r3)
	  (sicl-mir:make-register-location 'sicl-arm-assembler:r4)
	  (sicl-mir:make-register-location 'sicl-arm-assembler:r5)
	  (sicl-mir:make-register-location 'sicl-arm-assembler:r6)
	  (sicl-mir:make-register-location 'sicl-arm-assembler:r7)
	  (sicl-mir:make-register-location 'sicl-arm-assembler:r8)
	  (sicl-mir:make-register-location 'sicl-arm-assembler:r9)
	  (sicl-mir:make-register-location 'sicl-arm-assembler:r10)
	  (sicl-mir:make-register-location 'sicl-arm-assembler:r11)
	  (sicl-mir:make-register-location 'sicl-arm-assembler:r12)
	  (sicl-mir:make-register-location 'sicl-arm-assembler:SP)
	  (sicl-mir:make-register-location 'sicl-arm-assembler:LR)
	  (sicl-mir:make-register-location 'sicl-arm-assembler:PC)))

(defclass backend-arm ()
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Possibly convert MIR constants to immediates.

(defmethod sicl-program:convert-constant ((backend backend-arm) constant)
  (if (and (typep constant 'sicl-mir:constant-input)
	   (<= (- (expt 2 11)) (sicl-mir:value constant) (- (expt 2 11) 1)))
      (sicl-mir:make-immediate-input (sicl-mir:value constant))
      nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ARM LIR mixin classes.

(defclass conditional-execution-mixin ()
  ((%condition
    :initform 'sicl-arm-assembler:||
    :initarg :condition
    :reader condition)))

(defclass update-condition-flags-mixin ()
  ((%update-p
    :initform nil
    :initarg :update-p
    :reader update-p)))

(defclass immediate-input-mixin ()
  ((%immediate-value
    :initarg :immediate-value
    :reader immediate-value)))

(defclass register-input-mixin ()
  ((%shift-type
    :initform 'sicl-arm-assembler:LSL
    :initarg :shift-type
    :reader shift-type)
   (%shift-amount
    :initform 0
    :initarg :shift-amount
    :reader shift-amount)))

(defclass addressing-mixin ()
  ((%index-p
    :initform t
    :initarg :index-p
    :reader index-p)
   (%write-back-p
    :initform nil
    :initarg :write-back-p
    :reader write-back-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ARM LIR instruction ADD-IMMEDIATE-INSTRUCTION

(defclass add-immediate-instruction
    (sicl-mir:instruction
     conditional-execution-mixin
     update-condition-flags-mixin
     immediate-input-mixin)
  ())

(defmethod draw-instruction ((instruction add-immediate-instruction) stream)
  (format stream "   ~a [label = \"add ~a\"];~%"
	  (sicl-mir:unique-id instruction)
	  (immediate-value instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ARM LIR instruction ADD-REGISTER-INSTRUCTION

(defclass add-register-instruction
    (sicl-mir:instruction
     conditional-execution-mixin
     update-condition-flags-mixin
     register-input-mixin)
  ())

(defmethod draw-instruction ((instruction add-register-instruction) stream)
  (format stream "   ~a [label = \"add ~a ~a\"];~%"
	  (sicl-mir:unique-id instruction)
	  (if (and (eq (shift-type instruction) 'sicl-arm-assembler:LSL)
		   (= (shift-amount instruction) 0))
	      ""
	      (symbol-name (shift-type instruction)))
	  (if (and (eq (shift-type instruction) 'sicl-arm-assembler:LSL)
		   (= (shift-amount instruction) 0))
	      ""
	      (shift-amount instruction))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ARM LIR instruction SUB-IMMEDIATE-INSTRUCTION

(defclass sub-immediate-instruction
    (sicl-mir:instruction
     conditional-execution-mixin
     update-condition-flags-mixin
     immediate-input-mixin)
  ())

(defmethod draw-instruction ((instruction sub-immediate-instruction) stream)
  (format stream "   ~a [label = \"sub ~a\"];~%"
	  (sicl-mir:unique-id instruction)
	  (immediate-value instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ARM LIR instruction SUB-REGISTER-INSTRUCTION

(defclass sub-register-instruction
    (sicl-mir:instruction
     conditional-execution-mixin
     update-condition-flags-mixin
     register-input-mixin)
  ())

(defmethod draw-instruction ((instruction sub-register-instruction) stream)
  (format stream "   ~a [label = \"sub ~a ~a\"];~%"
	  (sicl-mir:unique-id instruction)
	  (if (and (eq (shift-type instruction) 'sicl-arm-assembler:LSL)
		   (= (shift-amount instruction) 0))
	      ""
	      (symbol-name (shift-type instruction)))
	  (if (and (eq (shift-type instruction) 'sicl-arm-assembler:LSL)
		   (= (shift-amount instruction) 0))
	      ""
	      (shift-amount instruction))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ARM LIR instruction RSB-IMMEDIATE
;;;
;;; This instruction is used to implement the MIR NEG-INSTRUCTION.

(defclass rsb-immediate-instruction
    (sicl-mir:instruction
     conditional-execution-mixin
     update-condition-flags-mixin
     immediate-input-mixin)
  ())

(defmethod draw-instruction ((instruction rsb-immediate-instruction) stream)
  (format stream "   ~a [label = \"rsb ~a\"];~%"
	  (sicl-mir:unique-id instruction)
	  (immediate-value instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ARM LIR instruction CMP-IMMEDIATE-INSTRUCTION

(defclass cmp-immediate-instruction
    (sicl-mir:instruction
     conditional-execution-mixin
     immediate-input-mixin)
  ())

(defmethod draw-instruction ((instruction cmp-immediate-instruction) stream)
  (format stream "   ~a [label = \"cmp ~a\"];~%"
	  (sicl-mir:unique-id instruction)
	  (immediate-value instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ARM LIR instruction CMP-REGISTER-INSTRUCTION

(defclass cmp-register-instruction
    (sicl-mir:instruction
     conditional-execution-mixin
     register-input-mixin)
  ())

(defmethod draw-instruction ((instruction cmp-register-instruction) stream)
  (format stream "   ~a [label = \"cmp ~a ~a\"];~%"
	  (sicl-mir:unique-id instruction)
	  (if (and (eq (shift-type instruction) 'sicl-arm-assembler:LSL)
		   (= (shift-amount instruction) 0))
	      ""
	      (symbol-name (shift-type instruction)))
	  (if (and (eq (shift-type instruction) 'sicl-arm-assembler:LSL)
		   (= (shift-amount instruction) 0))
	      ""
	      (shift-amount instruction))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ARM LIR instruction SELECT-INSTRUCTION
;;;
;;; This instruction does not correspond to a real ARM instruction.
;;; It typically follows a compare instruction, and is used to select
;;; a successor based on the result of that compare instruction.
;;;
;;; This instruction has two successors.

(defclass select-instruction (sicl-mir:instruction)
  ((%condition :initarg condition :reader condition)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ARM LIR instruction AND-IMMEDIATE-INSTRUCTION

(defclass and-immediate-instruction
    (sicl-mir:instruction
     conditional-execution-mixin
     update-condition-flags-mixin
     immediate-input-mixin)
  ())

(defmethod draw-instruction ((instruction and-immediate-instruction) stream)
  (format stream "   ~a [label = \"and ~a\"];~%"
	  (sicl-mir:unique-id instruction)
	  (immediate-value instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ARM LIR instruction AND-REGISTER-INSTRUCTION

(defclass and-register-instruction
    (sicl-mir:instruction
     conditional-execution-mixin
     update-condition-flags-mixin
     register-input-mixin)
  ())

(defmethod draw-instruction ((instruction and-register-instruction) stream)
  (format stream "   ~a [label = \"and ~a ~a\"];~%"
	  (sicl-mir:unique-id instruction)
	  (if (and (eq (shift-type instruction) 'sicl-arm-assembler:LSL)
		   (= (shift-amount instruction) 0))
	      ""
	      (symbol-name (shift-type instruction)))
	  (if (and (eq (shift-type instruction) 'sicl-arm-assembler:LSL)
		   (= (shift-amount instruction) 0))
	      ""
	      (shift-amount instruction))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ARM LIR instruction ORR-IMMEDIATE-INSTRUCTION

(defclass orr-immediate-instruction
    (sicl-mir:instruction
     conditional-execution-mixin
     update-condition-flags-mixin
     immediate-input-mixin)
  ())

(defmethod draw-instruction ((instruction orr-immediate-instruction) stream)
  (format stream "   ~a [label = \"orr ~a\"];~%"
	  (sicl-mir:unique-id instruction)
	  (immediate-value instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ARM LIR instruction ORR-REGISTER-INSTRUCTION

(defclass orr-register-instruction
    (sicl-mir:instruction
     conditional-execution-mixin
     update-condition-flags-mixin
     register-input-mixin)
  ())

(defmethod draw-instruction ((instruction orr-register-instruction) stream)
  (format stream "   ~a [label = \"orr ~a ~a\"];~%"
	  (sicl-mir:unique-id instruction)
	  (if (and (eq (shift-type instruction) 'sicl-arm-assembler:LSL)
		   (= (shift-amount instruction) 0))
	      ""
	      (symbol-name (shift-type instruction)))
	  (if (and (eq (shift-type instruction) 'sicl-arm-assembler:LSL)
		   (= (shift-amount instruction) 0))
	      ""
	      (shift-amount instruction))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ARM LIR instruction EOR-IMMEDIATE-INSTRUCTION

(defclass eor-immediate-instruction
    (sicl-mir:instruction
     conditional-execution-mixin
     update-condition-flags-mixin
     immediate-input-mixin)
  ())

(defmethod draw-instruction ((instruction eor-immediate-instruction) stream)
  (format stream "   ~a [label = \"eor ~a\"];~%"
	  (sicl-mir:unique-id instruction)
	  (immediate-value instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ARM LIR instruction EOR-REGISTER-INSTRUCTION

(defclass eor-register-instruction
    (sicl-mir:instruction
     conditional-execution-mixin
     update-condition-flags-mixin
     register-input-mixin)
  ())

(defmethod draw-instruction ((instruction eor-register-instruction) stream)
  (format stream "   ~a [label = \"eor ~a ~a\"];~%"
	  (sicl-mir:unique-id instruction)
	  (if (and (eq (shift-type instruction) 'sicl-arm-assembler:LSL)
		   (= (shift-amount instruction) 0))
	      ""
	      (symbol-name (shift-type instruction)))
	  (if (and (eq (shift-type instruction) 'sicl-arm-assembler:LSL)
		   (= (shift-amount instruction) 0))
	      ""
	      (shift-amount instruction))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ARM LIR instruction BLX-REGISTER-INSTRUCTION
;;;
;;; This instruction is used to implement the MIR FUNCALL instruction.

(defclass blx-register-instruction
    (sicl-mir:instruction
     conditional-execution-mixin)
  ())
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ARM LIR instruction BRANCH-IMMEDIATE-INSTRUCTION
;;;
;;; This instruction is used only after the code has been linearized.

(defclass branch-immediate-instruction
    (sicl-mir:instruction
     conditional-execution-mixin
     immediate-input-mixin)
  ())

(defmethod draw-instruction ((instruction branch-immediate-instruction) stream)
  (format stream "   ~a [label = \"branch ~a\"];~%"
	  (sicl-mir:unique-id instruction)
	  (immediate-value instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ARM LIR instruction BRANCH-REGISTER-INSTRUCTION
;;;
;;; This instruction is used to implement the MIR instructions
;;; RETURN-INSTRUCTION and TAILCALL-INSTRUCTION.

(defclass branch-register-instruction
    (sicl-mir:instruction
     conditional-execution-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ARM LIR instruction LDR-IMMEDIATE-INSTRUCTION

(defclass ldr-immediate-instruction
    (sicl-mir:instruction
     conditional-execution-mixin
     immediate-input-mixin
     addressing-mixin)
  ())

(defmethod draw-instruction ((instruction ldr-immediate-instruction) stream)
  (format stream "   ~a [label = \"ldr ~a\"];~%"
	  (sicl-mir:unique-id instruction)
	  (immediate-value instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ARM LIR instruction LDR-REGISTER-INSTRUCTION

(defclass ldr-register-instruction
    (sicl-mir:instruction
     conditional-execution-mixin
     register-input-mixin
     addressing-mixin)
  ())
     
(defmethod draw-instruction ((instruction ldr-register-instruction) stream)
  (format stream "   ~a [label = \"ldr ~a ~a\"];~%"
	  (sicl-mir:unique-id instruction)
	  (if (and (eq (shift-type instruction) 'sicl-arm-assembler:LSL)
		   (= (shift-amount instruction) 0))
	      ""
	      (symbol-name (shift-type instruction)))
	  (if (and (eq (shift-type instruction) 'sicl-arm-assembler:LSL)
		   (= (shift-amount instruction) 0))
	      ""
	      (shift-amount instruction))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ARM LIR instruction MOV-IMMEDIATE-INSTRUCTION

(defclass mov-immediate-instruction
    (sicl-mir:instruction
     conditional-execution-mixin
     update-condition-flags-mixin
     immediate-input-mixin)
  ())

(defmethod draw-instruction ((instruction mov-immediate-instruction) stream)
  (format stream "   ~a [label = \"mov ~a\"];~%"
	  (sicl-mir:unique-id instruction)
	  (immediate-value instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ARM LIR instruction MOV-REGISTER-INSTRUCTION

(defclass mov-register-instruction
    (sicl-mir:instruction
     conditional-execution-mixin
     update-condition-flags-mixin)
  ())

(defmethod draw-instruction ((instruction mov-register-instruction) stream)
  (format stream "   ~a [label = \"mov ~a ~a\"];~%"
	  (sicl-mir:unique-id instruction)
	  (if (and (eq (shift-type instruction) 'sicl-arm-assembler:LSL)
		   (= (shift-amount instruction) 0))
	      ""
	      (symbol-name (shift-type instruction)))
	  (if (and (eq (shift-type instruction) 'sicl-arm-assembler:LSL)
		   (= (shift-amount instruction) 0))
	      ""
	      (shift-amount instruction))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ARM LIR instruction MVN-IMMEDIATE-INSTRUCTION

(defclass mvn-immediate-instruction
    (sicl-mir:instruction
     conditional-execution-mixin
     update-condition-flags-mixin
     immediate-input-mixin)
  ())

(defmethod draw-instruction ((instruction mvn-immediate-instruction) stream)
  (format stream "   ~a [label = \"mvn ~a\"];~%"
	  (sicl-mir:unique-id instruction)
	  (immediate-value instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ARM LIR instruction MVN-REGISTER-INSTRUCTION

(defclass mvn-register-instruction
    (sicl-mir:instruction
     conditional-execution-mixin
     update-condition-flags-mixin)
  ())

(defmethod draw-instruction ((instruction mvn-register-instruction) stream)
  (format stream "   ~a [label = \"mvn ~a ~a\"];~%"
	  (sicl-mir:unique-id instruction)
	  (if (and (eq (shift-type instruction) 'sicl-arm-assembler:LSL)
		   (= (shift-amount instruction) 0))
	      ""
	      (symbol-name (shift-type instruction)))
	  (if (and (eq (shift-type instruction) 'sicl-arm-assembler:LSL)
		   (= (shift-amount instruction) 0))
	      ""
	      (shift-amount instruction))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ARM LIR instruction STR-IMMEDIATE-INSTRUCTION

(defclass str-immediate-instruction
    (sicl-mir:instruction
     conditional-execution-mixin
     immediate-input-mixin
     addressing-mixin)
  ())

(defmethod draw-instruction ((instruction str-immediate-instruction) stream)
  (format stream "   ~a [label = \"str ~a\"];~%"
	  (sicl-mir:unique-id instruction)
	  (immediate-value instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ARM LIR instruction STR-REGISTER-INSTRUCTION

(defclass str-register-instruction
    (sicl-mir:instruction
     conditional-execution-mixin
     register-input-mixin
     addressing-mixin)
  ())
     
(defmethod draw-instruction ((instruction str-register-instruction) stream)
  (format stream "   ~a [label = \"str ~a ~a\"];~%"
	  (sicl-mir:unique-id instruction)
	  (if (and (eq (shift-type instruction) 'sicl-arm-assembler:LSL)
		   (= (shift-amount instruction) 0))
	      ""
	      (symbol-name (shift-type instruction)))
	  (if (and (eq (shift-type instruction) 'sicl-arm-assembler:LSL)
		   (= (shift-amount instruction) 0))
	      ""
	      (shift-amount instruction))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Convert a MIR instruction graph to LIR.

(defgeneric convert-instruction (instruction))

(defmethod convert-instruction (instruction)
  (declare (ignore instruction))
  nil)

(defmethod sicl-program:convert-to-lir ((backend backend-arm) initial-instruction)
  (let ((table (make-hash-table :test #'eq)))
    (labels ((traverse (instruction)
	       (unless (gethash instruction table)
		 (setf (gethash instruction table) t)
		 (convert-instruction instruction))))
      (traverse initial-instruction))))

(defmethod convert-instruction ((instruction sicl-mir:get-argcount-instruction))
  (change-class instruction
		'mov-register-instruction
		:inputs (aref *registers* 3)))

(defmethod convert-instruction ((instruction sicl-mir:get-arg-instruction))
  (error "can't handle this yet"))

(defmethod convert-instruction ((instruction sicl-mir:==-instruction))
  (destructuring-bind (first second) (sicl-mir:inputs instruction)
    (let ((select (make-instance 'select-instruction
		    :condition 'sicl-arm-assembler:eq
		    :successors (sicl-mir:successors instruction))))
      (cond ((typep first 'sicl-mir:immediate-input)
	     (change-class instruction 'cmp-immediate-instruction
			   :inputs (list second first)
			   :successors (list select)))
	    ((typep second 'sicl-mir:immediate-input)
	     (change-class instruction 'cmp-immediate-instruction
			   :inputs (list first second)
			   :successors (list select)))
	    (t
	     (change-class instruction 'compare-register-instruction
			   :inputs (list first second)
			   :successors (list select)))))))

(defmethod convert-instruction ((instruction sicl-mir:u<-instruction))
  (destructuring-bind (first second) (sicl-mir:inputs instruction)
    (let ((successors (sicl-mir:successors instruction)))
      (cond ((typep first 'sicl-mir:immediate-input)
	     ;; We want to test whether IMM < REG, but the REG must
	     ;; come first, so we test whether REG > IMM.
	     (let ((select (make-instance 'select-instruction
			     :condition 'sicl-arm-assembler:HI
			     :successors successors)))
	       (change-class instruction 'cmp-immediate-instruction
			     :immediate-value (sicl-mir:value first)
			     :inputs (list second)
			     :successors (list select))))
	    ((typep second 'sicl-mir:immediate-input)
	     ;; We want to test whether REG < IMM.
	     (let ((select (make-instance 'select-instruction
			   :condition 'sicl-arm-assembler:CC
			   :successors successors)))
	       (change-class instruction 'cmp-immediate-instruction
			     :immediate-value (sicl-mir:value second)
			     :inputs (list first)
			     :successors (list select))))
	    (t
	     ;; We want to test whether REG1 < REG2.
	     (let ((select (make-instance 'select-instruction
			   :condition 'sicl-arm-assembler:CC
			   :successors successors)))
	       (change-class instruction 'cmp-register-instruction
			     :inputs (list first second)
			     :successors (list select))))))))

(defmethod convert-instruction ((instruction sicl-mir:u<=-instruction))
  (destructuring-bind (first second) (sicl-mir:inputs instruction)
    (let ((successors (sicl-mir:successors instruction)))
      (cond ((typep first 'sicl-mir:immediate-input)
	     ;; We want to test whether IMM <= REG, but the REG must
	     ;; come first, so we test whether REG >= IMM.
	     (let ((select (make-instance 'select-instruction
			     :condition 'sicl-arm-assembler:CS
			     :successors successors)))
	       (change-class instruction 'cmp-immediate-instruction
			     :immediate-value (sicl-mir:value first)
			     :inputs (list second)
			     :successors (list select))))
	    ((typep second 'sicl-mir:immediate-input)
	     ;; We want to test whether REG <= IMM.
	     (let ((select (make-instance 'select-instruction
			   :condition 'sicl-arm-assembler:LS
			   :successors successors)))
	       (change-class instruction 'cmp-immediate-instruction
			     :immediate-value (sicl-mir:value second)
			     :inputs (list first)
			     :successors (list select))))
	    (t
	     ;; We want to test whether REG1 <= REG2.
	     (let ((select (make-instance 'select-instruction
			   :condition 'sicl-arm-assembler:LS
			   :successors successors)))
	       (change-class instruction 'cmp-register-instruction
			     :inputs (list first second)
			     :successors (list select))))))))

(defmethod convert-instruction ((instruction sicl-mir:s<-instruction))
  (destructuring-bind (first second) (sicl-mir:inputs instruction)
    (let ((successors (sicl-mir:successors instruction)))
      (cond ((typep first 'sicl-mir:immediate-input)
	     ;; We want to test whether IMM < REG, but the REG must
	     ;; come first, so we test whether REG > IMM.
	     (let ((select (make-instance 'select-instruction
			     :condition 'sicl-arm-assembler:GT
			     :successors successors)))
	       (change-class instruction 'cmp-immediate-instruction
			     :immediate-value (sicl-mir:value first)
			     :inputs (list second)
			     :successors (list select))))
	    ((typep second 'sicl-mir:immediate-input)
	     ;; We want to test whether REG < IMM.
	     (let ((select (make-instance 'select-instruction
			   :condition 'sicl-arm-assembler:LT
			   :successors successors)))
	       (change-class instruction 'cmp-immediate-instruction
			     :immediate-value (sicl-mir:value second)
			     :inputs (list first)
			     :successors (list select))))
	    (t
	     ;; We want to test whether REG1 < REG2.
	     (let ((select (make-instance 'select-instruction
			   :condition 'sicl-arm-assembler:LT
			   :successors successors)))
	       (change-class instruction 'cmp-register-instruction
			     :inputs (list first second)
			     :successors (list select))))))))

(defmethod convert-instruction ((instruction sicl-mir:s<=-instruction))
  (destructuring-bind (first second) (sicl-mir:inputs instruction)
    (let ((successors (sicl-mir:successors instruction)))
      (cond ((typep first 'sicl-mir:immediate-input)
	     ;; We want to test whether IMM <= REG, but the REG must
	     ;; come first, so we test whether REG => IMM.
	     (let ((select (make-instance 'select-instruction
			     :condition 'sicl-arm-assembler:GE
			     :successors successors)))
	       (change-class instruction 'cmp-immediate-instruction
			     :immediate-value (sicl-mir:value first)
			     :inputs (list second)
			     :successors (list select))))
	    ((typep second 'sicl-mir:immediate-input)
	     ;; We want to test whether REG <= IMM.
	     (let ((select (make-instance 'select-instruction
			   :condition 'sicl-arm-assembler:LE
			   :successors successors)))
	       (change-class instruction 'cmp-immediate-instruction
			     :immediate-value (sicl-mir:value second)
			     :inputs (list first)
			     :successors (list select))))
	    (t
	     ;; We want to test whether REG1 <= REG2.
	     (let ((select (make-instance 'select-instruction
			   :condition 'sicl-arm-assembler:LE
			   :successors successors)))
	       (change-class instruction 'cmp-register-instruction
			     :inputs (list first second)
			     :successors (list select))))))))

;;; FIXME: handle carry
(defmethod convert-instruction ((instruction sicl-mir:u+-instruction))
  (destructuring-bind (first second) (sicl-mir:inputs instruction)
    (let ((successors (sicl-mir:successors instruction)))
      (ecase (length successors)
	(1 (cond ((typep first 'sicl-mir:immediate-input)
		  (change-class instruction 'add-immediate-instruction
				:immediate-value (sicl-mir:value first)
				:inputs (list second)))
		 ((typep second 'sicl-mir:immediate-input)
		  (change-class instruction 'add-immediate-instruction
				:immediate-value (sicl-mir:value second)
				:inputs (list first)))
		 (t
		  (change-class instruction 'add-register-instruction))))))))

;;; FIXME: handle carry
(defmethod convert-instruction ((instruction sicl-mir:u--instruction))
  (destructuring-bind (first second) (sicl-mir:inputs instruction)
    (let ((successors (sicl-mir:successors instruction)))
      (ecase (length successors)
	(1 (cond ((typep first 'sicl-mir:immediate-input)
		  (change-class instruction 'rsb-immediate-instruction
				:immediate-value (sicl-mir:value first)
				:inputs (list second)))
		 ((typep second 'sicl-mir:immediate-input)
		  (change-class instruction 'sub-immediate-instruction
				:immediate-value (sicl-mir:value second)
				:inputs (list first)))
		 (t
		  (change-class instruction 'sub-register-instruction))))))))

;;; FIXME: overflow
(defmethod convert-instruction ((instruction sicl-mir:s+-instruction))
  (destructuring-bind (first second) (sicl-mir:inputs instruction)
    (let ((successors (sicl-mir:successors instruction)))
      (ecase (length successors)
	(1 (cond ((typep first 'sicl-mir:immediate-input)
		  (change-class instruction 'add-immediate-instruction
				:immediate-value (sicl-mir:value first)
				:inputs (list second)))
		 ((typep second 'sicl-mir:immediate-input)
		  (change-class instruction 'add-immediate-instruction
				:immediate-value (sicl-mir:value second)
				:inputs (list first)))
		 (t
		  (change-class instruction 'add-register-instruction))))))))

;;; FIXME: handle overflow
(defmethod convert-instruction ((instruction sicl-mir:s--instruction))
  (destructuring-bind (first second) (sicl-mir:inputs instruction)
    (let ((successors (sicl-mir:successors instruction)))
      (ecase (length successors)
	(1 (cond ((typep first 'sicl-mir:immediate-input)
		  (change-class instruction 'rsb-immediate-instruction
				:immediate-value (sicl-mir:value first)
				:inputs (list second)))
		 ((typep second 'sicl-mir:immediate-input)
		  (change-class instruction 'sub-immediate-instruction
				:immediate-value (sicl-mir:value second)
				:inputs (list first)))
		 (t
		  (change-class instruction 'sub-register-instruction))))))))

(defmethod convert-instruction ((instruction sicl-mir:&-instruction))
  (destructuring-bind (first second) (sicl-mir:inputs instruction)
    (cond ((typep first 'sicl-mir:immediate-input)
	   (change-class instruction 'and-immediate-instruction
			 :immediate-value (sicl-mir:value first)
			 :inputs (list second)))
	  ((typep second 'sicl-mir:immediate-input)
	   (change-class instruction 'and-immediate-instruction
			 :immediate-value (sicl-mir:value second)
			 :inputs (list first)))
	  (t
	   (change-class instruction 'and-register-instruction)))))

(defmethod convert-instruction ((instruction sicl-mir:ior-instruction))
  (destructuring-bind (first second) (sicl-mir:inputs instruction)
    (cond ((typep first 'sicl-mir:immediate-input)
	   (change-class instruction 'orr-immediate-instruction
			 :immediate-value (sicl-mir:value first)
			 :inputs (list second)))
	  ((typep second 'sicl-mir:immediate-input)
	   (change-class instruction 'orr-immediate-instruction
			 :immediate-value (sicl-mir:value second)
			 :inputs (list first)))
	  (t
	   (change-class instruction 'orr-register-instruction)))))

(defmethod convert-instruction ((instruction sicl-mir:xor-instruction))
  (destructuring-bind (first second) (sicl-mir:inputs instruction)
    (cond ((typep first 'sicl-mir:immediate-input)
	   (change-class instruction 'eor-immediate-instruction
			 :immediate-value (sicl-mir:value first)
			 :inputs (list second)))
	  ((typep second 'sicl-mir:immediate-input)
	   (change-class instruction 'eor-immediate-instruction
			 :immediate-value (sicl-mir:value second)
			 :inputs (list first)))
	  (t
	   (change-class instruction 'eor-register-instruction)))))

(defmethod convert-instruction ((instruction sicl-mir:~-instruction))
  (let ((input (car (sicl-mir:inputs instruction))))
    (if (typep input 'sicl-mir:immediate-input)
	(change-class instruction 'mvn-immediate-instruction
		      :immediate-value (sicl-mir:value input))
	(change-class instruction 'mvn-register-instruction))))

		      
	  
