(cl:in-package #:cleavir-load-time-value-hoisting)

;;; Scanning is the process of associating every externalizable object with
;;; a suitable constructor.  Constructors of similar objects are coalesced
;;; in the process.

(defvar *compilation-environment*)

(defvar *data-constructors*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Similarity Tables
;;;
;;; Each of these tables maps from some key to the corresponding
;;; constructor.  The EQ table is used to ensure that each object is only
;;; scanned once.  The other tables are used for objects where the rules of
;;; similarity are less restrictive.

(defvar *eq-table*)

(defvar *eql-table*)

(defvar *equal-table*)

(defvar *equalp-table*)

(defmacro with-fresh-constructor-tables (&body body)
  `(let ((*eq-table* (make-hash-table :test #'eq))
         (*eql-table* (make-hash-table :test #'eql))
         (*equal-table* (make-hash-table :test #'equal))
         (*equalp-table* (make-hash-table :test #'equalp)))
     ,@body))

(defmacro constructor (key)
  `(gethash ,key *eq-table*))

(defun coalesce (object equalp-key)
  (let* ((equalp-key
           (list* (class-of object) equalp-key))
         (similar-constructor
           (gethash equalp-key *equalp-table*)))
    (if similar-constructor
        (setf (constructor object)
              similar-constructor)
        (setf (gethash equalp-key *equalp-table*)
              (constructor object)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Scanning

(defgeneric scan-hir (hir))

(defgeneric scan-datum (datum))

(defgeneric scan-constant (constant))

(defgeneric scan-load-time-value-form (form read-only-p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Scan HIR

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Scan Datum

(defmethod scan-datum ((load-time-value-input cleavir-ir:load-time-value-input))
  (scan-load-time-value-form
   (cleavir-ir:form load-time-value-input)
   (cleavir-ir:read-only-p load-time-value-input)))

(defmethod scan-datum ((constant-input cleavir-ir:constant-input))
  (let ((value (cleavir-ir:value constant-input)))
    (scan-constant value)))

(defmethod scan-datum ((constant-input cleavir-ir:immediate-input))
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Scan Constant

(defmethod scan-constant :around (datum)
  (call-next-method)
  (values))

(defmethod scan-constant (constant)
  (let ((constructor (constructor constant)))
    (cond
      ;; First visit - create a suitable constructor.
      ((not constructor)
       (multiple-value-bind (creation-form initialization-form)
           (make-load-form constant *compilation-environment*)
         (let* ((creation-thunk
                  (compile-hir creation-form))
                (initialization-thunk
                  (compile-hir initialization-thunk))
                (constructor
                  (make-instance 'user-defined-constructor
                    :creation-form creation-form
                    :creation-thunk creation-thunk
                    :initialization-form initialization-form
                    :initialization-thunk initialization-thunk)))
           (setf (constructor constant) constructor)
           (scan-hir creation-thunk)
           (setf (creation-form-finalized-p constructor) t)
           (scan-hir initialization-thunk))))
      ;; Recurring visit - check for creation form circularity.
      ((not (creation-form-finalized-p constructor))
       (error 'circular-dependencies-in-creation-form
              :prototype constant
              :creation-form (creation-form constructor))))))

(defmethod scan-constant ((number number))
  (with-memoization (number *eq-table*)
      (with-memoization (number *eql-table*)
          (make-instance 'number-constructor
            :prototype number))))

(defmethod scan-constant ((character character))
  (with-memoization (character *eq-table*)
      (with-memoization (character *eql-table*)
          (make-instance 'character-constructor
            :prototype character))))

(defmethod scan-constant ((symbol symbol))
  (with-memoization (symbol *eq-table*)
      (make-instance 'symbol-constructor
        :prototype symbol)
    (let ((symbol-name (symbol-name symbol))
          (symbol-package (symbol-package symbol))
          (current-package *package*))
      (scan-constant symbol-name)
      (scan-constant symbol-package)
      (scan-constant current-package)
      (coalesce symbol (list (constructor symbol-name)
                             (constructor symbol-package)))
      (unless (null symbol-package)
        (coalesce symbol (list (constructor symbol-name)
                               (constructor current-package)))))))

(defmethod scan-constant ((package package))
  (with-memoization (package *eq-table*)
      (make-instance 'package-constructor
        :prototype package)
    (let ((name (package-name package)))
      (scan-constant name)
      (coalesce package (constructor name)))))

(defmethod scan-constant ((random-state random-state))
  (with-memoization (random-state *eq-table*)
      (make-instance 'random-state-constructor
        :prototype random-state)))

(defmethod scan-constant ((cons cons))
  (with-memoization (cons *eq-table*)
      (make-instance 'cons-constructor
        :prototype cons)
    (scan-constant (car cons))
    (scan-constant (cdr cons))
    (coalesce cons (list (constructor (car cons))
                         (constructor (cdr cons))))))

(defmethod scan-constant ((array array))
  (with-memoization (array *eq-table*)
      (make-instance 'array-constructor
        :prototype array)
    (loop for index below (array-total-size array) do
      (scan-constant (row-major-aref array index)))
    (let ((key-array (make-array (array-total-size array))))
      (loop for index below (array-total-size array) do
        (setf (row-major-aref key-array index)
              (constructor (row-major-aref array index))))
      (coalesce array (list (array-rank array)
                            (array-dimensions array)
                            (array-element-type array)
                            key-array)))))

(defmethod scan-constant ((hash-table hash-table))
  (with-memoization (hash-table *eq-table*)
      (make-instance 'hash-table-constructor
        :prototype hash-table)
    (maphash (lambda (key value)
               (scan-constant key)
               (scan-constant value))
             hash-table)))

(defmethod scan-constant ((pathname pathname))
  (with-memoization (pathname *eq-table*)
      (with-memoization (pathname *equal-table*)
          (make-instance 'pathname-constructor
            :prototype pathname))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Load-Time-Value Scanning

(defmethod scan-load-time-value-form (form read-only-p)
  (with-memoization (form *eq-table*)
      (make-instance 'load-time-value-constructor
        :form form)))
