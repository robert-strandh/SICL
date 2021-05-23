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
  ;; Add this instruction as a using instruction to the new inputs.
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
  ;; Add this instruction as a defining instruction to the new outputs.
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

(defgeneric dynamic-environment-location (instruction))

(defgeneric (setf dynamic-environment-location) (new-location instruction))

(defmethod (setf dynamic-environment-location) :before
    (new-location instruction)
  ;; Remove this instruction as a using instruction from the
  ;; existing dynamic environment.
  (when (or (slot-boundp instruction '%dynamic-environment-location)
            (null (dynamic-environment-location instruction)))
    (let ((location (dynamic-environment-location instruction)))
      (setf (using-instructions location)
            (remove instruction (using-instructions location)
                    :test #'eq)))))

(defmethod (setf dynamic-environment-location) :after (new-location instruction)
  ;; Add this instruction as a defining instruction to the new dynamic environment.
  (push instruction (using-instructions new-location)))

(defclass instruction ()
  ((%predecessors :initform '() :initarg :predecessors :accessor predecessors)
   (%successors :initform '() :initarg :successors :accessor successors)
   (%inputs :initform '() :initarg :inputs :accessor inputs)
   (%outputs :initform '() :initarg :outputs :accessor outputs)
   (%dynamic-environment-location
    :initarg :dynamic-environment-location
    :accessor dynamic-environment-location)))

(defmethod initialize-instance :after
    ((obj instruction) &key input output successor dynamic-environment-location)
  (let ((inputs (if (null input) (inputs obj) (list input)))
        (outputs (if (null output) (outputs obj) (list output)))
        (successors (if (null successor) (successors obj) (list successor))))
    (reinitialize-instance obj :inputs inputs :outputs outputs :successors successors))
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
  ;; Add this instruction as a using instruction to its dynamic environment.
  (unless (null dynamic-environment-location)
    (push obj (using-instructions dynamic-environment-location)))
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
        :dynamic-environment-location (dynamic-environment-location instruction)))

(defun clone-instruction (instruction &rest initargs &key &allow-other-keys)
  (apply #'make-instance (class-of instruction) (append initargs (clone-initargs instruction))))

(defun nth-successor (n instruction)
  (let* ((successors (successors instruction))
         (result (nth n successors)))
    (assert (> (length successors) n))
    (check-type result instruction)
    result))

(defun (setf nth-successor) (successor n instruction)
  (let ((successors (successors instruction)))
    (assert (> (length successors) n))
    (check-type successor instruction)
    (setf (nth n successors) successor)))

(defun first-successor (instruction)
  (nth-successor 0 instruction))

(defun (setf first-successor) (successor instruction)
  (setf (nth-successor 0 instruction) successor))

(defun second-successor (instruction)
  (nth-successor 1 instruction))

(defun (setf second-successor) (successor instruction)
  (setf (nth-successor 1 instruction) successor))
