(cl:in-package #:cleavir-cst-to-ast)

(defun equal-representation (object)
  (multiple-value-bind (value present-p)
      (constant-record-cache object)
    (if present-p
        value
        object)))

(defun equalp-representation (object)
  (multiple-value-bind (value present-p)
      (constant-record-cache object)
    (if present-p
        value
        (if (characterp object)
            ;; For characters, we have to inhibit the behavior of EQUALP to
            ;; compare them with CHAR-EQUAL.
            (cons '.character. (char-int object))
            ;; Otherwise, we just use OBJECT itself as the key.
            object))))

;; The default - the object has no similarity keys.
(defmethod similarity-keys (client object)
  (values))

(defmethod similarity-keys (client (symbol symbol))
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

(defmethod similarity-keys (client (package package))
  `((,(class-of package)
     ,(equal-representation (package-name package)))))

(defmethod similarity-keys (client (cons cons))
  `((,(class-of cons)
     ,(equal-representation (car cons))
     ,(equal-representation (cdr cons)))))

(defmethod similarity-keys (client (array array))
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

(defmethod similarity-keys (client (string string))
  `((,(class-of string)
     ,(length string)
     ,(array-element-type string)
     ,string)))

(defmethod similarity-keys (client (bit-vector bit-vector))
  `((,(class-of bit-vector)
     ,(length bit-vector)
     ,bit-vector)))

(defmethod similarity-keys (client (hash-table hash-table))
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

(defmethod similarity-keys (client (pathname pathname))
  `((,(class-of pathname) ,pathname)))
