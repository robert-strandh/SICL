(cl:in-package #:sicl-hash-table)

(defgeneric hash-table-count (hash-table))

(defgeneric hash-table-rehash-size (hash-table))

(defgeneric hash-table-rehash-threshold (hash-table))

(defgeneric hash-table-size (hash-table))

(defvar *standard-tests*
  (loop for test in '(eq eql equal equalp)
        collect (cons test (fdefinition test))))

;; When the test is one of the four standardized hash table test functions,
;; the value returned must be a symbol, but it would be preferable for hash
;; table implementations to call a function instead of coercing a function
;; to a symbol and then coercing that back to a function.
(defgeneric %hash-table-test (hash-table))

(defun hash-table-test (hash-table)
  (let ((designator (%hash-table-test hash-table)))
    (if (symbolp designator)
        designator
        (let ((test-pair (rassoc designator *standard-tests*)))
          (if (null test-pair)
              designator
              (car test-pair))))))

(defgeneric gethash (key hash-table &optional default))

(defgeneric (setf gethash) (new-value key hash-table &optional default))

(defgeneric remhash (key hash-table))

(defgeneric maphash (function hash-table))

(defgeneric clrhash (hash-table))

;;; Internal
(defgeneric make-hash-table-iterator (hash-table))
