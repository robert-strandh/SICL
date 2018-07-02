(cl:in-package #:cleavir-load-time-value-hoisting)

;;; Scanning is the process of associating every externalizable object with
;;; a suitable constructor.  Constructors of similar objects are coalesced
;;; in the process.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Similarity Tables
;;;
;;; Each of these tables maps from some key to the corresponding
;;; constructor.  The EQL table is used to ensure that each object is only
;;; scanned once.  This works because if two objects are EQL, they are also
;;; similar in the sense of CLHS 3.2.4.2.2.  The EQUALP table is used to
;;; coalesce literal objects where the rules of similarity are less
;;; restrictive.  Finally

(defvar *eql-table*)

(defvar *equalp-table*)

(defmacro with-fresh-tables (&body body)
  `(let ((*eql-table* (make-hash-table :test #'eql))
         (*equalp-table* (make-hash-table :test #'equalp)))
     ,@body))

(defmacro constructor (key)
  `(gethash ,key *eql-table*))

;;; The function COALESCE ensures that for any two calls (coalesce o1 k1)
;;; and (coalesce o2 k2), where o1 and o2 are objects of the same type,
;;; (equalp k1 k2) implies (eq (constructor o1) (constructor o2)).
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
;;; Scan HIR

(defmethod scan-hir ((hir cleavir-ir:instruction) client)
  (cleavir-ir:map-instructions-arbitrary-order
   (lambda (instruction)
     (loop for input in (cleavir-ir:inputs instruction) do
       (scan-datum input client)))
   hir))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Scan Datum

(defmethod scan-datum (datum client)
  (values))

(defmethod scan-datum ((load-time-value-input cleavir-ir:load-time-value-input) client)
  (unless (constructor load-time-value-input)
    (let* ((form (cleavir-ir:form load-time-value-input))
           (read-only-p (cleavir-ir:read-only-p load-time-value-input))
           (creation-thunk (compile-form form client))
           (constructor
             (make-instance 'load-time-value-constructor
               :form form
               :creation-thunk creation-thunk
               :read-only-p read-only-p)))
      (setf (constructor load-time-value-input) constructor)
      (scan-hir (creation-thunk constructor) client))))

(defmethod scan-datum ((constant-input cleavir-ir:constant-input) client)
  (unless (constructor constant-input)
    (setf (constructor constant-input)
          (scan-literal-object
           (cleavir-ir:value constant-input)
           client))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Scan Constant

(defmethod scan-literal-object :around (object client)
  (multiple-value-bind (constructor present-p) (constructor object)
    (cond ((not present-p)
           (call-next-method)
           (constructor object))
          ((scanning-creation-form-p constructor)
           (error 'circular-dependencies-in-creation-form
                  :prototype object
                  :creation-form (creation-form constructor)))
          (t constructor))))

(defmethod scan-literal-object (object client)
  (multiple-value-bind (creation-form initialization-form)
      (make-load-form object *compilation-environment*)
    (let* ((creation-thunk (compile-form creation-form client))
           (initialization-thunk (compile-form initialization-form client))
           (constructor
             (make-instance 'user-defined-constructor
               :creation-form creation-form
               :creation-thunk creation-thunk
               :initialization-form initialization-form
               :initialization-thunk initialization-thunk)))
      (setf (constructor object) constructor)
      (scan-hir creation-thunk client)
      (setf (scanning-creation-form-p constructor) nil)
      (scan-hir initialization-thunk client))))

(defmethod scan-literal-object ((number number) client)
  (setf (constructor number)
        (make-standard-constructor number)))

(defmethod scan-literal-object ((character character) client)
  (setf (constructor character)
        (make-standard-constructor character)))

(defmethod scan-literal-object ((symbol symbol) client)
  (setf (constructor symbol)
        (make-standard-constructor symbol))
  (let ((symbol-name-constructor (scan-literal-object (symbol-name symbol) client))
        (symbol-package (symbol-package symbol)))
    (coalesce symbol (list symbol-name-constructor
                           (scan-literal-object symbol-package client)))
    (unless (null symbol-package)
      (coalesce symbol (list symbol-name-constructor
                             (scan-literal-object *package* client))))))

(defmethod scan-literal-object ((package package) client)
  (setf (constructor package)
        (make-standard-constructor package))
  (coalesce package (scan-literal-object (package-name package) client)))

(defmethod scan-literal-object ((random-state random-state) client)
  (setf (constructor random-state)
        (make-standard-constructor random-state)))

(defmethod scan-literal-object ((cons cons) client)
  (setf (constructor cons)
        (make-standard-constructor cons))
  (coalesce cons (list (scan-literal-object (car cons) client)
                       (scan-literal-object (cdr cons) client))))

(defmethod scan-literal-object ((array array) client)
  (setf (constructor array)
        (make-standard-constructor array))
  (let ((constructor-array (make-array (array-total-size array))))
    (loop for index below (array-total-size array) do
      (setf (aref constructor-array index)
            (scan-literal-object (row-major-aref array index) client)))
    (coalesce array (list (array-rank array)
                          (array-dimensions array)
                          (array-element-type array)
                          constructor-array))))

(defmethod scan-literal-object ((hash-table hash-table) client)
  (setf (constructor hash-table)
        (make-standard-constructor hash-table))
  (let ((constructor-table (make-hash-table)))
    (maphash
     (lambda (key value)
       (setf (gethash (scan-literal-object key client) constructor-table)
             (scan-literal-object value client)))
     hash-table)
    (coalesce hash-table (list (hash-table-test hash-table)
                               constructor-table))))

(defmethod scan-literal-object ((pathname pathname) client)
  (setf (constructor pathname)
        (make-standard-constructor pathname))
  (coalesce pathname pathname))
