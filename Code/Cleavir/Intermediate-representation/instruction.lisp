(cl:in-package #:cleavir-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instructions.

(defgeneric inputs (instruction))

(defgeneric (setf inputs) (new-inputs instruction))

(defmethod (setf inputs) :before (new-inputs instruction)
  (assert (listp new-inputs))
  (assert (every (lambda (input) (typep input 'datum)) new-inputs))
  ;; Remove this instruction as a using instruction from the existing
  ;; inputs.
  (loop for input in (inputs instruction)
	do (setf (using-instructions input)
		 (remove instruction (using-instructions input)
			 :test #'eq))))

(defmethod (setf inputs) :after (new-inputs instruction)
  ;; Add this instruction as a using instruction to the existing
  ;; inputs.
  (loop for input in (inputs instruction)
	do (push instruction (using-instructions input))))

;;; The operation (setf inputs (substitute new old inputs)) is quite
;;; common, so we provide a function for it. Both for cleanliness and
;;; efficiency.
(defgeneric substitute-input (new old instruction)
  (:argument-precedence-order instruction new old))

(defmethod substitute-input (new old instruction)
  ;; bypass the accessor to avoid the above methods.
  (setf (slot-value instruction '%inputs)
        (substitute new old (slot-value instruction '%inputs)))
  ;; Now maintain consistency.
  (setf (using-instructions old)
        (remove instruction (using-instructions old) :test #'eq))
  (pushnew instruction (using-instructions new))
  (values))

(defgeneric outputs (instruction))

(defgeneric (setf outputs) (new-outputs instruction))

(defmethod (setf outputs) :before (new-outputs instruction)
  (assert (listp new-outputs))
  (assert (every (lambda (output) (typep output 'datum)) new-outputs))
  ;; Remove this instruction as a defining instruction from the
  ;; existing outputs.
  (loop for output in (outputs instruction)
	do (setf (defining-instructions output)
		 (remove instruction (defining-instructions output)
			 :test #'eq))))

(defmethod (setf outputs) :after (new-outputs instruction)
  ;; Add this instruction as a defining instruction to the existing
  ;; outputs.
  (loop for output in (outputs instruction)
	do (push instruction (defining-instructions output))))

;; Like the above.
(defgeneric substitute-output (new old instruction)
  (:argument-precedence-order instruction new old))

(defmethod substitute-output (new old instruction)
  ;; bypass the accessor to avoid the above methods.
  (setf (slot-value instruction '%outputs)
        (substitute new old (slot-value instruction '%outputs)))
  ;; Now maintain consistency.
  (setf (defining-instructions old)
        (remove instruction (defining-instructions old) :test #'eq))
  (push instruction (defining-instructions new))
  (values))

;;; Default value, since it can be shared by several instructions
;;; during generation, similar to the AST case.
(defvar *policy*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Variable *ORIGIN*. Default for :origin initarg.
;;; This is useful because almost every instruction needs an origin, but they're
;;; shared very heavily and generated all over the place.

(defvar *origin*)

(defclass instruction ()
  ((%predecessors :initform '() :initarg :predecessors :accessor predecessors)
   (%successors :initform '() :initarg :successors :accessor successors)
   (%inputs :initform '() :initarg :inputs :accessor inputs)
   (%outputs :initform '() :initarg :outputs :accessor outputs)
   (%policy :initform *policy* :initarg :policy :accessor policy)
   (%origin :initform (if (boundp '*origin*) *origin* nil) :initarg :origin :accessor origin)))

(defmethod initialize-instance :after ((obj instruction) &key)
  (unless (and (listp (successors obj))
	       (every (lambda (successor)
			(typep successor 'instruction))
		      (successors obj)))
    (error "successors must be a list of instructions"))
  (assert (and (listp (inputs obj))
	       (every (lambda (input) (typep input 'datum)) (inputs obj))))
  (assert (and (listp (outputs obj))
	       (every (lambda (output) (typep output 'datum)) (outputs obj))))
  ;; Add this instruction as a using instruction to its inputs.
  (loop for input in (inputs obj)
	do (push obj (using-instructions input)))
  ;; Add this instruction as an assigning instruction to its outputs.
  (loop for output in (outputs obj)
	do (push obj (defining-instructions output)))
  ;; Add this instruction as a successor of its predecessors.
  (loop for successor in (successors obj)
	do (push obj (predecessors successor))))

;;; Returns an initialization args list suitable for MAKE-INSTANCE to reproduce
;;; the given instruction. Sssssort of like CLEAVIR-IO:SAVE-INFO.
(defgeneric clone-initargs (instruction)
  (:method-combination append))

(defmethod clone-initargs append ((instruction instruction))
  (list :inputs (inputs instruction)
        :outputs (outputs instruction)
        :predecessors (predecessors instruction)
        :successors (successors instruction)
        :policy (policy instruction)))

(defun clone-instruction (instruction &rest initargs &key &allow-other-keys)
  (apply #'make-instance (class-of instruction) (append initargs (clone-initargs instruction))))
