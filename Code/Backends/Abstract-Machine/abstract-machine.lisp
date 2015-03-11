;;; This is dumb of course, but this file is
;;; only a small test for now.
(cl:in-package #:sicl-compiler-phase-1)

;;; A static frame.  A static environment is a Lisp list of static
;;; frames. 
(defclass static-frame ()
  (;; Here we store the name and other information about
   ;; static frames of this type.  
   (descriptor :initform nil :initarg :descriptor :reader descriptor)
   ;; The values of the variables in this static frame in the form
   ;; of a Lisp vector
   (variables :initarg :variables :reader variables)))

;;; A list of static frames
(defparameter *static-environment* '())

;;; A dynamic frame.  A stack is a Lisp list of dynamic frames. 
(defclass dynamic-frame ()
  (;; A Lisp list of static frames. 
   (static-environment :initarg :static-environment :accessor static-environment)
   (return-instructions :initarg :return-instructions :reader return-instructions)
   (return-constants :initarg :return-constants :reader return-constants )
   (return-pc :initarg :return-pc :reader return-pc )))

;;; A list of dynamic frames. 
(defparameter *stack* '())

;;; A code object.  It contains the instructions to be executed
;;; and a vector of objects (we call it constants, but that's
;;; probably wrong) to be used by the code. 
(defclass code ()
  ((instructions :initarg :instructions :reader instructions)
   (constants :initarg :constants :reader constants)))

(defclass closure ()
  ((code :initarg :code :reader code)
   (static-environment :initarg :static-environment :reader static-environment)))

(defparameter *arguments* (make-array 50 :fill-pointer 0))

(defparameter *return-values* (make-array 20 :fill-pointer 0))

(defparameter *registers* (make-array 1000))

;;; The vector of instructions currently being executed.
(defparameter *instructions* nil)

;;; The vector of constants used by the current instructions.
(defparameter *constants* nil)

;;; The offset into the *instructions* vector of the next instruction
;;; to be executed. 
(defparameter *program-counter* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Global environments

;;; Make this one EQUAL for now so that we can store
;;; function names like (SETF ELT). 
(defparameter *global-functions* (make-hash-table :test #'equal))

(defparameter *global-variables* (make-hash-table :test #'eq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instructions

(defclass instruction () ())

(defclass single-register-instruction-mixin ()
  ((register :initarg :register :reader register)))

(defgeneric execute (instruction))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; HALT instruction

(defclass halt-instruction (instruction) ())

(defmethod execute ((instruction halt-instruction))
  (throw 'halt nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; LISP instruction.
;;;
;;; This is the universal cheater instruction.  We put arbitrary
;;; Lisp code in a function that we execute. 

(defclass lisp-instruction (instruction)
  ((lisp-function :initarg lisp-function :reader lisp-function)))

(defmethod exectute ((instruction lisp-instruction))
  (funcall (lisp-function instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CALL instruction.
;;;
;;; This instruction is used to call a full closure. 
;;; A closure contains a static environment in which the closure
;;; was created, and that environment is established by this
;;; instruction.  The code of the closure is then responsible
;;; for creating new levels on top of that static environment
;;; if this is required.  

(defclass call-instruction (single-register-instruction-mixin) ())

(defmethod execute ((instruction call-instruction))
  (let ((closure (aref *registers* (register instruction))))
    (push (make-instance 'dynamic-frame
	    :static-environment *static-environment*
	    :return-instruction *instructions*
	    :return-constants *constants*
	    :return-pc *program-counter*)
	  *stack*)
    (setf *static-environment* (static-environment closure))
    (setf *instructions* (instructions (code closure)))
    (setf *constants* (constants (code closure)))
    (setf *program-counter* 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CALL-CODE instruction.
;;;
;;; This instruction is used to call a code object directly,
;;; without turning into a closure first.  It is used when 
;;; the code object is always called from code in its lexically
;;; enclosing environment, so that the environment to be used
;;; for this call is the same as that of its caller. 

(defclass call-code-instruction (single-register-instruction-mixin) ())

(defmethod execute ((instruction call-code-instruction))
  (let ((code (aref *registers* (register instruction))))
    (push (make-instance 'dynamic-frame
	    :static-environment *static-environment*
	    :return-instruction *instructions*
	    :return-constants *constants*
	    :return-pc *program-counter*)
	  *stack*)
    (setf *instructions* (instructions code))
    (setf *constants* (constants code))
    (setf *program-counter* 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; RETURN instruction

(defclass return-instruction (instruction) ())

(defmethod execute ((instruction return-instruction))
  (let ((dynamic-frame (car *stack*)))
    (setf *static-environment* (static-environment dynamic-frame))
    (setf *instructions* (return-instructions dynamic-frame))
    (setf *constants* (return-constants dynamic-frame))
    (setf *program-counter* (return-pc dynamic-frame)))
  (pop *stack*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; LOAD-IMMEDIATE instruction
;;;
;;; This instruction is used when the value to be loaded
;;; can be contained in the instruction itself.  This
;;; is typically the case for fixnums, characters, and
;;; perhaps some more. 

(defclass load-immediate-instruction (single-register-instruction-mixin)
  ((immediate :initarg :immediate :reader immediate)))

(defmethod execute ((instruction load-immediate-instruction))
  (setf (aref *registers* (register instruction))
	(immediate instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; LOAD-CONSTANT instruction
;;;
;;; This inststruction is used when the value to be loaded 
;;; is contained in the *constants* vector.  This is the
;;; case for structurally complicated compile-time values
;;; such as quoted lists, code objects, etc. 

(defclass load-constant-instruction (single-register-instruction-mixin)
  ((constant-index :initarg :constant-index :reader constant-index)))

(defmethod execute ((instruction load-constant-instruction))
  (setf (aref *registers* (register instruction))
	(aref *constants* (constant-index instruction))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; LOAD-VARIABLE instruction
;;;

(defclass load-variable-instruction (single-register-instruction-mixin)
  ((environment-level :initarg :environment-level :reader environment-level)
   (variable-index :initarg :variable-index :reader variable-index)))

(defmethod execute ((instruction load-variable-instruction))
  (setf (aref *registers* (register instruction))
	(aref (nth (environment-level instruction)
		   *static-environment*)
	      (variable-index instruction))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; STORE-VARIABLE instruction
;;;

(defclass store-variable-instruction (single-register-instruction-mixin)
  ((environment-level :initarg :environment-level :reader environment-level)
   (variable-index :initarg :variable-index :reader variable-index)))

(defmethod execute ((instruction store-variable-instruction))
  (setf (aref (nth (environment-level instruction)
		   *static-environment*)
	      (variable-index instruction))
	(aref *registers* (register instruction))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The processor cycle

(defun processor-cycle ()
  (let ((instruction (aref *instructions* *program-counter*)))
    (incf *program-counter*)
    (execute instruction)))

(defun run ()
  (catch 'halt
    (loop do (processor-cycle))))

