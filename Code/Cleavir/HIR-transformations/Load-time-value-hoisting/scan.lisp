(cl:in-package #:cleavir-load-time-value-hoisting)

;;; Scanning is the process of associating every externalizable object with
;;; a suitable constructor.  Constructors of similar objects are coalesced
;;; in the process.

(defvar *compilation-environment*)

(defvar *data-constructors*)

(defvar *eq-table*)

(defvar *eql-table*)

(defvar *equal-table*)

;;; Invoke SCAN-DATUM once for each datum in HIR.
(defgeneric scan-hir (hir))

;;; Invoke the appropriate scanner for DATUM and create an appropriate
;;; entry in *DATA-CONSTRUCTORS*.
(defgeneric scan-datum (datum))

;;; Ensure that CONSTANT has a corresponding constructor in the *EQ-TABLE*.
(defgeneric scan-constant (constant))

;;; Ensure that FORM has a corresponding constructor in the *EQ-TABLE*.
(defgeneric scan-load-time-value-form (form))

;;; Scan all dependencies of CONSTRUCTOR.  If two coalescible constructors
;;; have the same type and EQUAL dependencies, replace the *EQ-TABLE* value
;;; of the object of one of them with the other constructor.
(defgeneric scan-dependencies (constructor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Scan HIR

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Scan Datum

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Scan Constant

(defmethod scan-constant :around (constant)
  (unless (gethash constant *eq-table*)
    (let ((constructor (call-next-method)))
      (setf (gethash constant *eq-table*) constructor)
      (scan-dependencies constructor)))
  (values))

(defmethod scan-constant (constant)
  (multiple-value-bind (creation-form initialization-form)
      (make-load-form constant *compilation-environment*)
    ;; TODO compile and recurse.
    (make-instance 'user-defined-constructor
      :creation-form creation-form
      :initialization-form initialization-form)))

(defmethod scan-constant ((number number))
  (with-memoization (number *eql-table*)
    (make-instance 'number-constructor
      :object number)))

(defmethod scan-constant ((character character))
  (with-memoization (character *eql-table*)
    (make-instance 'character-constructor
      :object character)))

(defmethod scan-constant ((symbol symbol))
  (make-instance 'symbol-constructor
    :object symbol))

(defmethod scan-constant ((package package))
  (make-instance 'package-constructor
    :object package))

(defmethod scan-constant ((random-state random-state))
  (make-instance 'random-state-constructor
    :object random-state))

(defmethod scan-constant ((cons cons))
  (make-instance 'cons-constructor
    :object cons))

(defmethod scan-constant ((array array))
  (make-instance 'array-constructor
    :object array))

(defmethod scan-constant ((hash-table hash-table))
  (make-instance 'hash-table-constructor
    :object hash-table))

(defmethod scan-constant ((pathname pathname))
  (with-memoization (pathname *equal-table*)
    (make-instance 'pathname-constructor
      :object pathname)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Load-Time-Value Scanning

(defmethod scan-load-time-value-form (form)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Scan Dependencies

(defun constructor (object)
  (scan-constant object)
  (values (gethash object *eq-table*)))

(defmethod scan-dependencies :around (object)
  (call-next-method)
  (values))

(defmethod scan-dependencies :around ((constructor coalescible-constructor))
  (let* ((equal-key
           (list* (class-of constructor)
                  (call-next-method)))
         (similar-constructor
           (gethash equal-key *equal-table*)))
    (if similar-constructor
        (setf (gethash (object constructor) *eq-table*)
              similar-constructor)
        (setf (gethash equal-key *equal-table*)
              constructor))))

(defmethod scan-dependencies ((symbol-constructor symbol-constructor))
  (let ((symbol-name (symbol-name symbol))
        (home-package (symbol-package symbol))
        (current-package *package*))
    (cond ((not home-package)
           (list (constructor symbol-name)))
          ((find-symbol symbol-name current-package)
           (list (constructor symbol-name)
                 (constructor current-package)))
          (t
           (list (constructor symbol-name)
                 (constructor home-package))))))

(defmethod scan-dependencies ((package-constructor package-constructor))
  (list (constructor (package-name package))))

(defmethod scan-dependencies ((cons-constructor cons-constructor))
  (list (constructor (car cons))
        (constructor (cdr cons))))

(defmethod scan-dependencies ((array-constructor array-constructor))
  (list* (constructor (array-rank array))
         (mapcar #'constructor (array-dimensions array))
         (constructor (array-element-type array))
         (map 'list #'constructor
              (make-array (array-total-size array) :displaced-to array))))
