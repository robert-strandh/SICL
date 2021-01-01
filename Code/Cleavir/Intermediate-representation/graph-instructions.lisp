(in-package #:cleavir-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instructions core to the understanding of graph traversal.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction ENTER-INSTRUCTION.
;;;
;;; This instruction encapsulates all the implementation-specific
;;; machinery involved in verifying the argument count and parsing the
;;; arguments.  It has a single successor.

(defclass enter-instruction (instruction one-successor-mixin)
  ((%lambda-list :initarg :lambda-list :accessor lambda-list)
   ;; The number of closure cells this function has.
   ;; Used internally, but shouldn't matter to code generation.
   (%closure-size :initarg :closure-size :accessor closure-size
                  :initform 0 :type (integer 0))))

(defgeneric static-environment (instruction))
(defmethod static-environment ((instruction enter-instruction))
  (first (outputs instruction)))

(defgeneric dynamic-environment-output (instruction))
(defmethod dynamic-environment-output ((instruction enter-instruction))
  (second (outputs instruction)))

(defgeneric parameters (instruction))
(defmethod parameters ((instruction enter-instruction))
  (cddr (outputs instruction)))

(defun make-enter-instruction
    (lambda-list dynenv &key (successor nil successor-p))
  (let* ((outputs (loop for item in lambda-list
                        append (cond ((member item lambda-list-keywords) '())
                                     ((consp item)
                                      (if (= (length item) 3)
                                          (cdr item)
                                          item))
                                     (t (list item))))))
    (make-instance 'enter-instruction
      :lambda-list lambda-list
      ;; We add an additional output that will hold the static
      ;; environment.
      :outputs (list* (new-temporary) dynenv outputs)
      :successors (if successor-p (list successor) '())
      :dynamic-environment-location dynenv)))

(defmethod clone-initargs append ((instruction enter-instruction))
  (list :lambda-list (lambda-list instruction)
        :closure-size (closure-size instruction)))

;;; Maintain consistency of lambda list with outputs.
(defmethod substitute-output :after (new old (instruction enter-instruction))
  (setf (lambda-list instruction)
        (subst new old (lambda-list instruction) :test #'eq)))

(defmethod (setf outputs) :before (new-outputs (instruction enter-instruction))
  (let ((old-lambda-outputs (parameters instruction))
        (new-lambda-outputs (cddr new-outputs)))
    ;; FIXME: Not sure what to do if the new and old outputs are different lengths.
    ;; For now we're silent.
    (setf (lambda-list instruction)
          (sublis (mapcar #'cons old-lambda-outputs new-lambda-outputs)
                  (lambda-list instruction)
                  :test #'eq))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction ENCLOSE-INSTRUCTION.

(defclass enclose-instruction (instruction one-successor-mixin
                               allocation-mixin)
  ((%code :initarg :code :accessor code)
   ;; Points to the instruction which initializes the closure environment.
   (%initializer :initarg :initializer :accessor initializer :initform nil)))

(defmethod clone-initargs append ((instruction enclose-instruction))
  (list :code (code instruction) :initializer (initializer instruction)))
