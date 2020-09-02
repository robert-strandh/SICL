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

(defclass enter-instruction (one-successor-mixin instruction)
  ((%lambda-list :initarg :lambda-list :accessor lambda-list)
   ;; An alist from output locations to lists of declaration specifiers.
   ;; Since SPECIAL is handled elsewhere, these declarations should only
   ;; be relevant for optimization, and may be discarded if necessary.
   (%bound-declarations :initarg :bound-declarations :initform nil
                        :accessor bound-declarations)
   ;; The number of closure cells this function has.
   ;; Used internally, but shouldn't matter to code generation.
   (%closure-size :initarg :closure-size :accessor closure-size
                  :initform 0 :type (integer 0))
   ;; For debug/introspection
   (%name :initarg :name :initform nil :reader name)
   (%docstring :initarg :docstring :initform nil :reader docstring)
   (%original-lambda-list :initarg :original-lambda-list :initform nil
                          :reader original-lambda-list)
   ;; default KLUDGE
   (%attributes :initarg :attributes :initform 0 :reader attributes)))

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
    (lambda-list dynenv &key (successor nil successor-p) origin
                          name docstring bound-declarations
                          original-lambda-list (attributes 0))
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
      :dynamic-environment dynenv
      :name name :docstring docstring
      :original-lambda-list original-lambda-list
      :bound-declarations bound-declarations
      :attributes attributes
      :origin origin)))

(defmethod clone-initargs append ((instruction enter-instruction))
  (list :lambda-list (lambda-list instruction)
        :bound-declarations (bound-declarations instruction)
        :closure-size (closure-size instruction)
        :name (name instruction)
        :docstring (docstring instruction)
        :original-lambda-list (original-lambda-list instruction)
        :attributes (attributes instruction)))

;;; Maintain consistency of lambda list and declarations with outputs.
(defmethod substitute-output :after (new old (instruction enter-instruction))
  (setf (lambda-list instruction)
        (subst new old (lambda-list instruction) :test #'eq)
        (bound-declarations instruction)
        (loop for (location . decls) in (bound-declarations instruction)
              if (eq location old)
                collect (cons new decls)
              else collect (cons location decls))))

(defmethod (setf outputs) :before (new-outputs (instruction enter-instruction))
  (let* ((old-lambda-outputs (parameters instruction))
         (new-lambda-outputs (cddr new-outputs))
         (map (mapcar #'cons old-lambda-outputs new-lambda-outputs)))
    ;; FIXME: Not sure what to do if the new and old outputs are different lengths.
    ;; For now we're silent.
    (setf (lambda-list instruction)
          (sublis map (lambda-list instruction) :test #'eq)
          (bound-declarations instruction)
          (loop for (location . decls) in (bound-declarations instruction)
                for pair = (assoc location map :test #'eq)
                if pair
                  collect (cons (cdr pair) decls)
                else collect (cons location decls)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction ENCLOSE-INSTRUCTION.

(defclass enclose-instruction (one-successor-mixin allocation-mixin
                               instruction)
  ((%code :initarg :code :accessor code)
   ;; Points to the instruction which initializes the closure environment.
   (%initializer :initarg :initializer :accessor initializer :initform nil)))  

(defun make-enclose-instruction (output successor code)
  (make-instance 'enclose-instruction
    :outputs (list output)
    :successors (list successor)
    :code code))

(defmethod clone-initargs append ((instruction enclose-instruction))
  (list :code (code instruction) :initializer (initializer instruction)))
