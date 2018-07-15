(cl:in-package #:cleavir-load-time-value-hoisting)

;;; Two objects are considered similar if their EQUALP-KEYS share at least
;;; one entry that is equalp.  The following methods encode the similarity
;;; rules of CLHS 3.2.4.2.2, with the exception of random states.

(defmethod equalp-keys (object system)
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
