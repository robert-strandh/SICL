(cl:in-package #:cleavir-load-time-value-hoisting)

;;; Scanning is the process of associating every externalizable object with
;;; a suitable constructor.  Constructors of similar objects are coalesced
;;; in the process.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Similarity
;;;
;;; Each of these tables maps from some key to the corresponding
;;; constructor.  The EQL table is used to ensure that each object is only
;;; scanned once.  This works because if two objects are EQL, they are also
;;; similar in the sense of CLHS 3.2.4.2.2.  The EQUALP table is used to
;;; coalesce literal objects where the rules of similarity are less
;;; restrictive.

(defvar *eql-table*)

(defvar *equalp-table*)

(defmacro with-constructor-tables (&body body)
  `(let ((*eql-table* (make-hash-table :test #'eql))
         (*equalp-table* (make-hash-table :test #'equalp)))
     ,@body))

(defmacro constructor (key)
  `(gethash ,key *eql-table*))

;;; The function COALESCE ensures that for any two calls (coalesce o1 k1)
;;; and (coalesce o2 k2), where o1 and o2 are objects of the same type,
;;; (equalp k1 k2) implies (eq (constructor o1) (constructor o2)).
(defun coalesce (object equalp-key)
  (let ((similar-constructor (gethash equalp-key *equalp-table*)))
    (if similar-constructor
        (setf (constructor object)
              similar-constructor)
        (setf (gethash equalp-key *equalp-table*)
              (constructor object)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Scanning

(defmethod scan-hir ((hir cleavir-ir:instruction) system)
  (cleavir-ir:map-instructions-arbitrary-order
   (lambda (instruction)
     (loop for datum in (cleavir-ir:inputs instruction)
           unless (immediate-p datum system) do
             (ensure-constructor datum system)))
   hir))

(defmethod ensure-constructor (object system)
  (multiple-value-bind (constructor present-p) (constructor object)
    (cond ((not present-p)
           (let* ((constructor (make-constructor object system))
                  (creation-form (creation-form constructor))
                  (initialization-form (initialization-form constructor)))
             (setf (constructor object) constructor)
             ;; Compile and scan the constructor's creation form.
             (when creation-form
               (let ((creation-thunk
                       (compile-form creation-form system)))
                 (setf (creation-thunk constructor)
                       creation-thunk)
                 (scan-hir creation-thunk system)))
             (setf (creation-form-finalized-p constructor) t)
             ;; Compile and scan the constructor's initialization form.
             (when initialization-form
               (let  ((initialization-thunk
                        (compile-form initialization-form system)))
                 (setf (initialization-thunk constructor)
                       initialization-thunk)
                 (scan-hir initialization-thunk system)))
             ;; Attempt to coalesce this constructor, i.e., replace it with
             ;; an existing constructor of a similar object.
             (loop for equalp-key in (equalp-keys object system) do
               (coalesce object equalp-key))
             (constructor object)))
          ((not (creation-form-finalized-p constructor))
           (error 'circular-dependencies-in-creation-form
                  :object object
                  :creation-form (creation-form constructor)))
          (t constructor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Equalp Keys

(defmethod equalp-keys ((object t) system)
  '())

(defmethod equalp-keys ((symbol symbol) system)
  (let* ((class (class-of symbol))
         (name (symbol-name symbol))
         (package (symbol-package symbol))
         (name-constructor (constructor name))
         (package-constructor (constructor package))
         (keys '()))
    (push (list class name-constructor package-constructor) keys)
    (unless (null package)
      (push (list class name-constructor (constructor *package*)) keys))
    keys))

(defmethod equalp-keys ((package package) system)
  `((,(class-of package)
     ,(constructor (package-name package)))))

(defmethod equalp-keys ((cons cons) system)
  `((,(class-of cons)
     ,(constructor (car cons))
     ,(constructor (cdr cons)))))

(defmethod equalp-keys ((array array) system)
  `((,(class-of array)
     ,(array-rank array)
     ,(array-dimensions array)
     ,(array-element-type array)
     ,(let ((constructor-array (make-array (array-total-size array))))
        (loop for index below (array-total-size array) do
          (setf (aref constructor-array index)
                (constructor (row-major-aref array index))))
        constructor-array))))

(defmethod equalp-keys ((hash-table hash-table) system)
  `((,(class-of hash-table) ,(hash-table-test hash-table)
     ,(let ((constructor-table (make-hash-table)))
           (maphash
            (lambda (key value)
              (setf (gethash (constructor key) constructor-table)
                    (constructor value)))
            hash-table)
           constructor-table))))

(defmethod equalp-keys ((pathname pathname) system)
  `((,(class-of pathname)
     ,pathname)))

