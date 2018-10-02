(cl:in-package #:cleavir-load-time-value-hoisting)

;;; Two objects are considered similar if their SIMILARITY-KEYS share at least
;;; one entry that is equalp.  The following methods encode the similarity
;;; rules of CLHS 3.2.4.2.2, with the exception of random states.

(defmethod similarity-keys (object system)
  (values))

(defmethod similarity-keys ((symbol symbol) system)
  (let* ((class (class-of symbol))
         (name (symbol-name symbol))
         (package (symbol-package symbol))
         (name-representation (equal-representation name))
         (package-representation (equal-representation package))
         (equal-keys '()))
    ;; Symbols with similar name and similar package are similar. Also,
    ;; apparently uninterned symbols with the same name are similar.  This
    ;; first key takes care of both cases.
    (push (list class name-representation package-representation)
          equal-keys)
    ;; Two interned symbols can also be similar if they are both accessible
    ;; in the current package at compile time.
    (unless (null package)
      (push (list class name-representation (equalp-representation *package*))
            equal-keys))
    equal-keys))

(defmethod similarity-keys ((package package) system)
  `((,(class-of package)
     ,(equal-representation (package-name package)))))

(defmethod similarity-keys ((cons cons) system)
  `((,(class-of cons)
     ,(equal-representation (car cons))
     ,(equal-representation (cdr cons)))))

(defmethod similarity-keys ((array array) system)
  (values
   `()
   `((,(class-of array)
      ,(array-rank array)
      ,(array-dimensions array)
      ,(array-element-type array)
      ,(let ((key-array (make-array (array-total-size array))))
         (loop for index below (array-total-size array) do
           (setf (aref key-array index)
                 (equalp-representation (row-major-aref array index))))
         key-array)))))

(defmethod similarity-keys ((string string) system)
  `((,(class-of string)
     ,(length string)
     ,(array-element-type string)
     ,string)))

(defmethod similarity-keys ((bit-vector bit-vector) system)
  `((,(class-of bit-vector)
     ,(length bit-vector)
     ,bit-vector)))

(defmethod similarity-keys ((hash-table hash-table) system)
  (values
   `()
   `((,(class-of hash-table)
      ,(hash-table-test hash-table)
      ,(let ((key-table (make-hash-table :test #'equalp)))
         (maphash
          (lambda (key value)
            (setf (gethash (equalp-representation key) key-table)
                  (equalp-representation value)))
          hash-table)
         key-table)))))

(defmethod similarity-keys ((pathname pathname) system)
  `((,(class-of pathname) ,pathname)))
