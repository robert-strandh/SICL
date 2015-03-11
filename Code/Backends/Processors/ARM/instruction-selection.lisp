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
;;; This instruction is used to implement the MIR FUNCALL-INSTRUCTION.

(defclass blx-register-instruction
    (sicl-mir:instruction
     conditional-execution-mixin)
  ())
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ARM LIR instruction BX-INSTRUCTION
;;;
;;; This instruction is used to implement the MIR RETURN-INSTRUCTION.

(defclass bx-instruction
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ARM LIR instruction POP-FP-INSTRUCTION

(defclass pop-fp-instruction
    (sicl-mir:instruction
     conditional-execution-mixin)
  ())


(defmethod draw-instruction ((instruction pop-fp-instruction) stream)
  (format stream "   ~a [label = \"popfp\"];~%"
	  (sicl-mir:unique-id instruction)))
     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Convert a MIR instruction graph to LIR.

(defvar *linkage-rack-lexical*)
(defvar *code-object-lexical*)
(defvar *return-address-lexical*)
(defvar *static-environment-lexical*)

(defgeneric convert-instruction (instruction))

(defmethod convert-instruction (instruction)
  (declare (ignore instruction))
  nil)

(defun convert-instruction-graph (initial-instruction)
  (let ((table (make-hash-table :test #'eq))
	(*linkage-rack-lexical* (sicl-mir:new-temporary))
	(*code-object-lexical* (sicl-mir:new-temporary))
	(*return-address-lexical* (sicl-mir:new-temporary))
	(*static-environment-lexical* (sicl-mir:new-temporary)))
    (cleavir-ir:map-instructions-arbitrary-order
     #'convert-instruction
     initial-instruction)))

(defmethod sicl-program:convert-to-lir ((backend backend-arm) initial-instruction)
  (convert-instruction-graph initial-instruction))

;;; FIXME: check the constants
(defmethod convert-instruction ((instruction sicl-mir:enter-instruction))
  (setf (sicl-mir:outputs instruction)
	(list (aref *registers* 0)
	      (aref *registers* 1)
	      (aref *registers* 2)
	      (aref *registers* 3)
	      (aref *registers* 9)
	      (aref *registers* 11)
	      (aref *registers* 12)
	      (aref *registers* 13)
	      (aref *registers* 14)))
  ;; Generate code for moving the retrurn address to a lexical
  ;; variable.
  (setf (sicl-mir:successors instruction)
	(list (sicl-mir:make-assignment-instruction
	       (aref *registers* 14)
	       *return-address-lexical*
	       (sicl-mir:successors instruction))))
  ;; Generate code for moving the code object argument to a lexical
  ;; variable.
  (setf (sicl-mir:successors instruction)
	(list (sicl-mir:make-assignment-instruction
	       (aref *registers* 12)
	       *code-object-lexical*
	       (sicl-mir:successors instruction))))
  ;; Generate code for moving the static environment argument to a
  ;; lexical variable.
  (setf (sicl-mir:successors instruction)
	(list (sicl-mir:make-assignment-instruction
	       (aref *registers* 3)
	       *code-object-lexical*
	       (sicl-mir:successors instruction))))
  ;; Generate code for loading the linkage rack from the code
  ;; object.
  (let ((temp (sicl-mir:new-temporary)))
    (setf (sicl-mir:successors instruction)
	  (list (make-instance 'ldr-immediate-instruction
		  :inputs (list (aref *registers* 12))
		  :immediate-value 1
		  :outputs (list temp)
		  :successors
		  (list (make-instance 'ldr-immediate-instruction
			  :inputs (list temp)
			  :immediate-value 12
			  :outputs (list *linkage-rack-lexical*)
			  :successors (sicl-mir:successors instruction))))))))

;;; To convert the argcount, take the difference between FP and SP.
(defmethod convert-instruction ((instruction sicl-mir:get-argcount-instruction))
  (change-class instruction
		'sub-register-instruction
		:inputs (list (aref *registers* 11) (aref *registers* 13))))

(defmethod convert-instruction ((instruction sicl-mir:get-arg-instruction))
  (let ((input (car (sicl-mir:inputs instruction))))
    (if (typep input 'sicl-mir:immediate-input)
	(let ((value (sicl-mir:value input)))
	  (if (<= value 8)
	      (change-class instruction 'mov-register-instruction
			    :inputs (list (aref *registers* (/ value 4))))
	      (change-class instruction 'ldr-immediate-instruction
			    :immediate-value value
			    :inputs (list (aref *registers* 13)))))
	(change-class instruction 'ldr-register-instruction
		      :inputs (list (aref *registers* 13) input)))))

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

(defmethod convert-instruction ((instruction sicl-mir:neg-instruction))
  (change-class instruction 'rsb-immediate-instruction
		:immediate-value 0))

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

(defmethod convert-instruction ((instruction sicl-mir:memref-instruction))
  (let* ((inputs (sicl-mir:inputs instruction))
	 (offset (if (= (length inputs) 2) (sicl-mir:value (cadr inputs)) 0)))
    (change-class instruction 'ldr-immediate-instruction
		  :immediate-input offset
		  :inputs (list (car inputs)))))

(defmethod convert-instruction ((instruction sicl-mir:memset-instruction))
  (let* ((inputs (sicl-mir:inputs instruction))
	 (offset (if (= (length inputs) 3) (sicl-mir:value (caddr inputs)) 0)))
    (change-class instruction 'ldr-immediate-instruction
		  :immediate-input offset
		  :inputs (subseq inputs 0 2))))

(defmethod convert-instruction ((instruction sicl-mir:funcall-instruction))
  (let ((callee (car (sicl-mir:inputs instruction)))
	(arguments (cdr (sicl-mir:inputs instruction)))
	(last-instruction instruction)
	;; SUCCESSOR is always the GET-VALUES-INSTRUCTION that follows
	;; the FUNCALL-INSTRUCTION.
	(successor (sicl-mir:successors instruction)))
    ;; We start by subtracting from SP N+1 words where N is the
    ;; number of arguments to pass.  The additional word is for the
    ;; saved FP.  We leave FP intact so that we can easily access the
    ;; data of the current stack frame without changing the offset.
    (change-class instruction 'sub-immediate-instruction
		  :immediate-input (* 4 (1+ (length arguments)))
		  :inputs (list (aref *registers* 13)))
    ;; Next, we assign to the registers containing the first three
    ;; arguments.  We use a generic assignment instruction because we
    ;; don't know where the register allocator will put the lexical
    ;; for the argument.
    (loop for argument in arguments
	  for i from 0 below 3
	  do (let ((inst (sicl-mir:make-assignment-instruction
			  argument (aref *registers* i) successor)))
	       (setf (sicl-mir:successors last-instruction)
		     (list inst))
	       (setf last-instruction inst)))
    ;; Now we deal with the arguments starting with the fourth one.
    (when (> (length arguments) 3)
      (loop for argument in (subseq arguments 3)
	    for offset from 12 by 4
	    do (let ((inst (make-instance 'str-immediate-instruction
			     :immediate-input offset
			     :inputs (list argument (aref *registers* 13))
			     :successors (list successor))))
		 (setf (sicl-mir:successors last-instruction)
		       (list inst))
		 (setf last-instruction inst))))
    ;; Almost done.  Store the current value of FP in its place
    (let ((inst (make-instance 'str-immediate-instruction
		  :immediate-input (* 4 (length arguments))
		  :inputs (list (aref *registers* 11) (aref *registers* 13))
		  :successors (list successor))))
      (setf (sicl-mir:successors last-instruction)
	    (list inst))
      (setf last-instruction inst))
    ;; Set the calle FP value.
    (let ((inst (make-instance 'add-immediate-instruction
		  :immediate-input (* 4 (length arguments))
		  :inputs (list (aref *registers* 11) (aref *registers* 13))
		  :successors (list successor))))
      (setf (sicl-mir:successors last-instruction)
	    (list inst))
      (setf last-instruction inst))
    ;; Finally, do the BLX.
    (let ((inst (make-instance 'blx-register-instruction
		  :inputs (list callee)
		  :outputs (list
			    (aref *registers* 0)
			    (aref *registers* 1)
			    (aref *registers* 2)
			    (aref *registers* 3)
			    (aref *registers* 14))
		  :successors (list successor))))
      (setf (sicl-mir:successors last-instruction)
	    (list inst))
      (setf last-instruction inst))))

;;; FIXME: handle more than 3 arguments.
(defmethod convert-instruction ((instruction sicl-mir:tailcall-instruction))
  (let ((callee (car (sicl-mir:inputs instruction)))
	(arguments (cdr (sicl-mir:inputs instruction)))
	(last-instruction instruction)
	;; SUCCESSOR is always the GET-VALUES-INSTRUCTION that follows
	;; the FUNCALL-INSTRUCTION.
	(successor (sicl-mir:successors instruction)))
    ;; This gets messy when there are more than 3 arguments to pass
    ;; so we don't handle that at the moment.
    (when (> (length arguments) 3)
      (error "can handle at most 3 arguments in a tailcall at the moment"))
    ;; We assign to the registers containing the first three
    ;; arguments.  We use a generic assignment instruction because we
    ;; don't know where the register allocator will put the lexical
    ;; for the argument.
    (loop for argument in arguments
	  for i from 0 below 3
	  do (let ((inst (sicl-mir:make-assignment-instruction
			  argument (aref *registers* i) successor)))
	       (setf (sicl-mir:successors last-instruction)
		     (list inst))
	       (setf last-instruction inst)))
    ;; Adjust SP so that it reflects the number of arguments that we
    ;; pass
    (let ((inst (make-instance 'sub-immediate-instruction
		  :immediate-input (* 4 (length arguments))
		  :inputs (list (aref *registers* 11))
		  :outputs (list (aref *registers* 13))
		  :successors (list successor))))
      (setf (sicl-mir:successors last-instruction)
	    (list inst))
      (setf last-instruction inst))
    ;; Finally, do the BLX.
    (let ((inst (make-instance 'blx-register-instruction
		  :inputs (list callee)
		  :outputs (list
			    (aref *registers* 0)
			    (aref *registers* 1)
			    (aref *registers* 2)
			    (aref *registers* 3)
			    (aref *registers* 14))
		  :successors (list successor))))
      (setf (sicl-mir:successors last-instruction)
	    (list inst))
      (setf last-instruction inst))))

;;; We don't have all the information we need for the
;;; ENCLOSE-INSTRUCTION yet, in particular the offset into the code
;;; where the entry point is located.  Ultimately, the enclose
;;; instruction will be converted to a function call that takes the
;;; current static environment, the code object, and an offset as
;;; arguments.  For now, we just add the static environment and the
;;; code object as inputs to the instruction so that the register
;;; allocator can do its thing.
(defmethod convert-instruction ((instruction sicl-mir:enclose-instruction))
  (convert-instruction-graph (car (sicl-mir:inputs instruction)))
  (push *static-environment-lexical* (sicl-mir:inputs instruction))
  (push *code-object-lexical* (sicl-mir:inputs instruction)))

;;; FIXME: we only handle no values or one value at the moment.
(defmethod convert-instruction ((instruction sicl-mir:get-values-instruction))
  (unless (< (length (sicl-mir:outputs instruction)) 2)
    (error "can only handle 0 or 1 return value at the moment"))
  (if (= (length (sicl-mir:outputs instruction)) 1)
      (change-class instruction 'sicl-mir:assignment-instruction
		    :inputs (list (aref *registers* 0)))
      (change-class instruction 'sicl-mir:nop-instruction)))

;;; FIXME: we only handle at most three return values at the moment
(defmethod convert-instruction ((instruction sicl-mir:return-instruction))
  (let ((return-values (sicl-mir:inputs instruction))
	(last-instruction instruction)
	(successor (car (sicl-mir:successors instruction))))
    (when (> (length return-values) 3)
      (error "we handle at most 3 return values at the moment."))
    (if (= (length return-values) 0)
	(change-class instruction 'mov-register-instruction
		      :inputs (list (aref *registers* 11))
		      :outputs (list (aref *registers* 13)))
	(progn
	  (change-class instruction 'sicl-mir:assignment-instruction
			:inputs (list (car return-values))
			:outputs (list (aref *registers* 0)))
	  (loop for return-value in (cdr return-values)
		for reg from 1
		do (let ((inst (sicl-mir:make-assignment-instruction
				return-value (aref *registers* reg) successor)))
		     (setf (sicl-mir:successors last-instruction)
			   (list inst))
		     (setf last-instruction inst)))
	  (let ((inst (make-instance 'mov-register-instruction
			:inputs (list (aref *registers* 11))
			:outputs (list (aref *registers* 13))
			:successors (list successor))))
	    (setf (sicl-mir:successors last-instruction)
		  (list inst))
	    (setf last-instruction inst))))
    ;; Restore the return address.
    ;; FIXME: it could probably be restored in any register.
    (let ((inst (sicl-mir:make-assignment-instruction
		 *return-address-lexical* (aref *registers* 14) successor)))
      (setf (sicl-mir:successors last-instruction)
	    (list inst))
      (setf last-instruction inst))
    (let ((inst (make-instance 'pop-fp-instruction
		  :successors (list successor))))
      (setf (sicl-mir:successors last-instruction)
	    (list inst))
      (setf last-instruction inst))
    (let ((inst (make-instance 'bx-instruction
		  :inputs (aref *registers* 14)
		  :successors (list successor))))
      (setf (sicl-mir:successors last-instruction)
	    (list inst))
      (setf last-instruction inst))))
