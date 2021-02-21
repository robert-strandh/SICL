(cl:in-package #:sicl-hash-table)

(defvar *default-hash-table-class*)

(defmethod initialize-instance :after ((hash-table hash-table) &key test)
  (assert (> (hash-table-rehash-size hash-table) 1)
          ()
          "The rehash-size must be greater than 1.")
  ;; Ensure the hash table test is a function.
  (etypecase test
    (function
     (setf (%%hash-table-test hash-table) test))
    (symbol
     (setf (%%hash-table-test hash-table)
           (fdefinition (hash-table-test hash-table))))))

(defun make-hash-table (&rest rest
                        &key (test 'eql) size rehash-size rehash-threshold
                             (class *default-hash-table-class*)
                        &allow-other-keys)
  (declare (ignore test size rehash-size rehash-threshold))
  (apply #'make-instance class
         (append (loop for (keyword value) on rest by #'cddr
                       unless (eql keyword :class)
                         collect keyword and collect value))))
